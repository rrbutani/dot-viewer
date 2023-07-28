use std::{
    any,
    ops::{Range, RangeInclusive},
};

use rhai::{
    Array, CustomType, Dynamic, EvalAltResult, FnPtr, ImmutableString, NativeCallContext, INT,
};

use super::{Ref, RhaiResult};

pub type ArrayRef<'r, T> = Ref<'r, [T]>;

/*
pub struct ArrayRefPackage<T: Clone>(PhantomData<T>);

// Note: we cannot just use the rhai `def_package` and `export_module` macros
// because we are trying to be generic over some `T`; we do not have module
// level generics in Rust (yet!)

macro_rules! for_pkg_deps {
    (|$ty_alias:ident| $block:expr) => {
        for_pkg_deps! {
            @(
                BasicArrayPackage,
            ) => |$ty_alias| $block
        }
    };

    (@($($pkg_dep_ty:ty),* $(,)?) => |$ty_alias:ident| $block:expr) => {
        $({
            type $ty_alias = $pkg_dep_ty;

            $block
        })*
    };
}

impl<T: Clone> Package for ArrayRefPackage<T> {
    fn init(module: &mut Module) {
        for_pkg_deps!(|X| X::init(module));

        module.set_native_fn(name, func)
    }

    fn as_shared_module(&self) -> Shared<Module> {
        let mut module = Module::new();
        Self::init(&mut module);

        Shared::new(module)
    }

    fn init_engine(engine: &mut rhai::Engine) {
        for_pkg_deps!(|X| X::init_engine(engine));

        engine.register_global_module(module)

        engine.register_type_with_name(name)
    }
}
*/

// A copy of `rhai::eval::target::calc_offset_len` which is, unfortuantely, not
// public.
#[allow(clippy::absurd_extreme_comparisons, clippy::unnecessary_cast)]
pub fn calc_offset_len(length: usize, start: INT, len: INT) -> (usize, usize) {
    let start = if start < 0 {
        let abs_start = start.unsigned_abs();
        if abs_start as u64 > /* rhai::MAX_USIZE_INT */ INT::MAX as u64 {
            0
        } else {
            length - usize::min(abs_start as usize, length)
        }
    } else if start > /* crate::MAX_USIZE_INT */ INT::MAX || start as usize >= length {
        return (length, 0);
    } else {
        start as usize
    };

    let len = if len <= 0 {
        0
    } else if len > /* crate::MAX_USIZE_INT */ INT::MAX || len as usize > length - start {
        length - start
    } else {
        len as usize
    };

    (start, len)
}

impl<T: Clone + ?Sized> CustomType for ArrayRef<'static, T>
where
    // TODO: what to do about these restrictions?
    // we don't want to impose them on everyone; we don't want to gate __all__
    // the methods on this..
    //
    // lack of specialization makes this tricky
    ArrayRef<'static, T>: PartialEq,
    T: PartialEq,
{
    fn build(mut builder: rhai::TypeBuilder<Self>) {
        // TODO: it'd be nice to be able to stick this stuff in a module
        // named `array_ref::<ty_name>::` but we'll leave that for another day

        // TODO: if we later drop down to using the `Module` API directly we can
        // insert doc strings for these functions...

        // TODO: this API also doesn't let us communicate _purity_ of the
        // functions...

        builder.with_name("ArrayRefOf"); // TODO: need static string..

        // The subset of this API that applies to immutable array references:
        // https://rhai.rs/book/language/arrays.html

        ////////////////////////////////////////////////////////////////////////

        let get = |this: &mut Self, idx: rhai::INT| -> Result<Ref<'static, T>, Dynamic> {
            let idx = if idx < 0 {
                match this.len().checked_sub(idx.unsigned_abs() as usize) {
                    Some(x) => x,
                    None => return Err(Dynamic::UNIT),
                }
            } else {
                idx as usize
            };

            // SAFETY: we guard this with a witness..
            let borrowed: &'static [T] = unsafe { this.get_unchecked_reference() };
            let item: &'static T = borrowed.get(idx).ok_or(Dynamic::UNIT)?;

            Ok(Ref::new(item, this.get_witness_copy()))
        };

        builder
            // `get`: returns a cloned value
            .with_fn("get", move |this: &mut Self, idx: rhai::INT| -> Dynamic {
                let (Ok(x) | Err(x)) =
                    get(this, idx).map(|x| x.get().clone()).map(Dynamic::from::<T>);
                x
            })
            // `get_borrowed`: returns a `Ref`
            .with_fn("get_borrowed", move |this: &mut Self, idx: rhai::INT| -> Dynamic {
                let (Ok(x) | Err(x)) = get(this, idx).map(Dynamic::from);
                x
            });

        ////////////////////////////////////////////////////////////////////////

        builder
            // `+` (array concat): returns an owned `Array`
            //
            // we support both borrowed arrays and owned arrays on the rhs
            .with_fn("+", |this: &mut Self, that: Self| -> Array {
                let this = this.iter().cloned().map(Dynamic::from::<T>);
                let that = that.iter().cloned().map(Dynamic::from::<T>);

                this.chain(that).collect()
            })
            .with_fn("+", |this: &mut Self, that: Array| -> Array {
                let this = this.iter().cloned().map(Dynamic::from::<T>);

                this.chain(that.into_iter()).collect()
            });

        ////////////////////////////////////////////////////////////////////////

        let eq_arr = |ctx: NativeCallContext, this: &mut Self, that: Array| -> RhaiResult<bool> {
            // we're not going to be friendly here and try to fudge the types;
            // you must have an exact match
            //
            // i.e. comparing a `ArrayRef<&'static str>` with an `Array` of
            // `ImmutableString`s will produce an error!
            //
            // note that we're not using `OP_EQUALS` and `call_native_fn`
            // because we don't want to come up with a way to shove a reference
            // to our arg into a `Dynamic`...
            if this.len() != that.len() {
                return Ok(false);
            }

            for (this, that) in this.iter().zip(that.iter()) {
                match that.read_lock::<T>() {
                    Some(that) => {
                        if this != &*that {
                            return Ok(false);
                        }
                    }
                    None => {
                        // TODO: Could clone here and call out to `OP_EQUALS`
                        // to be more friendly...
                        return Err(Box::new(EvalAltResult::ErrorMismatchDataType(
                            that.type_name().to_string(),      // actual
                            any::type_name::<T>().to_string(), // expected
                            ctx.position(),
                        )));
                    }
                }
            }

            Ok(true)
        };
        builder
            // ==
            .with_fn("==", |this: &mut Self, that: Self| *this == that)
            .with_fn("==", eq_arr)
            // !=
            .with_fn("!=", |this: &mut Self, that: Self| *this != that)
            .with_fn("!=", move |ctx: NativeCallContext, this: &mut Self, that| {
                eq_arr(ctx, this, that).map(|x| !x)
            });

        ////////////////////////////////////////////////////////////////////////

        // TODO: offer `_borrowed` variants?
        builder
            // pop
            .with_fn("pop", |this: &mut Self| -> Dynamic {
                // safety: no reference escape; we clone `last`
                match unsafe { this.get_unchecked_reference() }.split_last() {
                    Some((last, rest)) => {
                        this.set(rest);
                        Dynamic::from::<T>(last.clone())
                    }
                    None => Dynamic::UNIT,
                }
            })
            // shift
            .with_fn("shift", |this: &mut Self| -> Dynamic {
                // safety: no reference escape; we clone `first`
                match unsafe { this.get_unchecked_reference() }.split_first() {
                    Some((first, rest)) => {
                        this.set(rest);
                        Dynamic::from::<T>(first.clone())
                    }
                    None => Dynamic::UNIT,
                }
            });

        ////////////////////////////////////////////////////////////////////////

        let extract_with_start_and_len = |this: &mut Self, start: INT, len: INT| -> Self {
            let (start, len) = calc_offset_len(this.get().len(), start, len);

            this.mk_derived(|this| {
                // safety: reference is guarded by a witness
                let this = unsafe { this.get_unchecked_reference() };
                &this[start..][..len]
            })
        };

        builder
            // extract(start)
            .with_fn("extract", move |this: &mut Self, start: INT| -> Self {
                extract_with_start_and_len(this, start, INT::MAX)
            })
            // extract(start, len)
            .with_fn("extract", extract_with_start_and_len)
            // extract(start, range-inclusive)
            .with_fn("extract", move |this: &mut Self, range: RangeInclusive<INT>| -> Self {
                let start = INT::max(*range.start(), 0);
                let end = INT::max(*range.end(), start);
                extract_with_start_and_len(this, start, end - start + 1)
            })
            // extract(start, range-exclusive)
            .with_fn("extract", move |this: &mut Self, range: Range<INT>| -> Self {
                let start = INT::max(range.start, 0);
                let end = INT::max(range.end, start);
                extract_with_start_and_len(this, start, end - start)
            });

        ////////////////////////////////////////////////////////////////////////

        builder
            // len (also property)
            .with_fn("len", |this: &mut Self| -> INT { this.get().len().try_into().unwrap() })
            .with_get("len", |this: &mut Self| -> INT { this.get().len().try_into().unwrap() })
            // is_empty (also property)
            .with_fn("is_empty", |this: &mut Self| this.get().is_empty())
            .with_get("is_empty", |this: &mut Self| this.get().is_empty());

        ////////////////////////////////////////////////////////////////////////

        builder
            // truncate
            .with_fn("truncate", |this: &mut Self, len: INT| {
                let len: usize = INT::max(len, 0).try_into().unwrap();
                let len = usize::min(len, this.get().len());

                // safety: reference is guarded by a witness
                this.set(&unsafe { this.get_unchecked_reference() }[0..len])
            })
            // chop
            .with_fn("chop", |this: &mut Self, len: INT| {
                let len = INT::max(len, 0).try_into().unwrap();
                let len = usize::min(len, this.get().len());
                let offset = this.get().len() - len;

                // safety: reference is guarded by a witness
                this.set(&unsafe { this.get_unchecked_reference() }[offset..])
            })
            // split
            .with_fn("split", |this: &mut Self, index: INT| -> Self {
                let split_point = if index < 0 {
                    let index: usize = index.unsigned_abs().try_into().unwrap();
                    this.get().len().saturating_sub(index)
                } else {
                    let index: usize = index.try_into().unwrap();
                    index.min(this.get().len())
                };

                // safety: both references are guarded by a witness
                this.mk_derived(|this| {
                    let (new_this, that) =
                        unsafe { this.get_unchecked_reference() }.split_at(split_point);
                    this.set(new_this);
                    that
                })
            });

        ////////////////////////////////////////////////////////////////////////

        // TODO: offer a borrowed variant?

        builder
            // for_each
            //
            // NOTE: we clone the items before handing them off..
            // this makes the semantics of the function different; you cannot
            // mutate..
            .with_fn("for_each", |ctx: NativeCallContext, this: &mut Self, func: FnPtr| -> RhaiResult<()> {
                for (i, item) in this.get().iter().enumerate() {
                    let optional_idx_arg = [(i as INT).into()];
                    let mut item_copy = Dynamic::from::<T>(item.clone());

                    let _  = func.call_raw_with_extra_args("map", &ctx, Some(&mut item_copy), [], optional_idx_arg, None)?;

                    // Check to see if the item has been mutated:
                    let item_copy = item_copy.read_lock::<T>().unwrap();
                    if *item_copy != *item {
                        // TODO: test!
                        return Err(Box::new(EvalAltResult::ErrorRuntime(
                            Dynamic::from::<ImmutableString>(
                                ImmutableString::from(format!(
                                    "Error when calling `for_each` function `{}`: attempted to modify element at index {i}. `ArrayRef<{}>` is immutable!",
                                    func.fn_name(),
                                    any::type_name::<T>(),
                                ))
                            ),
                        ctx.position())))
                    }
                }

                Ok(())
            })

        ////////////////////////////////////////////////////////////////////////

            // filter

            // contains

            // index_of(elem)
            // index_of(elem, start_pos)
            // index_of(fn)
            // index_of(fn, start_pos)

            // find(fn)
            // find(fn, start_pos)

            // find_map(fn)
            // find_map(fn, start_pos)

            // map

            // reduce(fn)
            // reduce(fn, initial_val)

            // reduce_rev(fn)
            // reduce_rev(fn, initial_val)

            // some

            // all

        ;

        // .is_iterable()
        // indexer get

        // debug
        // display
        // ;
    }
}

#[cfg(test)]
mod tests {
    use rhai::{Dynamic, Engine, Scope, INT};
    use std::{fmt::Debug, mem};

    use super::*;

    fn test<T: Clone, R: Debug + Clone + 'static, E: Debug + Clone + 'static + PartialEq<R>>(
        inp: ArrayRef<'static, T>,
        expected_res: E,
        _return_type_witness: impl FnOnce() -> R, // cannot name the type of an expression so we do this instead.. (lazy)
        exp: &str,
    ) where
        ArrayRef<'static, T>: CustomType,
    {
        let mut engine = Engine::new();
        engine.build_type::<ArrayRef<'static, T>>();

        let mut scope = Scope::new();
        scope.push("arr", inp);

        let result: R = engine.eval_expression_with_scope::<R>(&mut scope, exp).unwrap();
        assert_eq!(expected_res, result)
    }

    macro_rules! test {
        ($(#[$meta:meta])* $name:ident: $array:expr $(, 'ty($arr_ty:ty))? => $script:literal $(as ($ret_ty:ty))? == $expected_result:expr) => {
            #[test]
            $(#[$meta])*
            fn $name() {
                $(
                    let inp: &'static $arr_ty = &$array;

                    #[cfg(disabled)]
                )?
                let inp = &{ $array };


                let inp = ArrayRef::from(inp);

                $(
                    let _return_type_witness = || -> $ret_ty {
                        todo!()
                    };
                    #[cfg(disabled)]
                )?
                let _return_type_witness = || {
                    $expected_result
                };

                test(inp, $expected_result, _return_type_witness, $script)
            }
        };
    }

    macro_rules! tests {
        (($name:ident)
            $(
                $(#[$meta:meta])* $test_name:ident: $array:expr $(, 'ty($arr_ty:ty))? =>
                    $script:literal $(as ($ret_ty:ty))? == $expected_result:expr
            ),* $(,)?
        ) => {
            mod $name {
                #[allow(unused)]
                use super::*;
                $(
                    test! {
                        $(#[$meta])* $test_name: $array $(, 'ty($arr_ty))? =>
                            $script $(as ($ret_ty))? == $expected_result
                    }
                )*
            }
        };
    }

    #[rustfmt::skip]
    #[allow(unused)]
    macro_rules! d { ($e:expr) => { Dynamic::from($e) }; }

    #[repr(transparent)]
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct ComparableDynamic<Actual>(Actual);

    // impl<A> From<A> for ComparableDynamic<A> {
    //     fn from(value: A) -> Self {
    //         ComparableDynamic(value)
    //     }
    // }

    impl<A> ComparableDynamic<A> {
        pub fn into_inner(self) -> A {
            self.0
        }
    }

    impl<A: Clone + 'static> TryFrom<Dynamic> for ComparableDynamic<A> {
        type Error = ();

        fn try_from(value: Dynamic) -> Result<Self, Self::Error> {
            let val = value.read_lock::<A>().ok_or(())?;
            Ok(Self(val.clone()))
        }
    }

    // Note: this is an alternative to `into_typed_array` (i.e. you don't have a
    // Dynamic on your hands, you have an `Array`)
    impl<A: Clone + 'static> TryFrom<Array> for ComparableDynamic<Vec<A>> {
        type Error = ();

        fn try_from(value: Array) -> Result<Self, Self::Error> {
            Ok(Self(
                value
                    .into_iter()
                    .map(|x| x.try_into().map(|x: ComparableDynamic<A>| x.into_inner()))
                    .collect::<Result<Vec<A>, ()>>()?,
            ))
        }
    }

    impl<A: PartialEq + Clone + 'static> PartialEq<Dynamic> for ComparableDynamic<A> {
        fn eq(&self, other: &Dynamic) -> bool {
            if other.is_string() {
                let s = other.read_lock::<rhai::ImmutableString>().unwrap();

                // For want of specialization...
                //
                // this is sketchy but it's fine for tests
                match any::type_name::<A>() {
                    "&str" => return unsafe { mem::transmute_copy::<A, &str>(&self.0) } == **s,
                    "String" | "std::string::String" => {
                        let this = unsafe { mem::transmute::<_, &String>(&self) };
                        return *this == **s;
                    }
                    _ => {}
                }
            }

            let val = other.read_lock::<A>().unwrap_or_else(|| {
                panic!(
                    "expected type {}, got value `{:?}` with type `{}`",
                    any::type_name::<A>(),
                    other,
                    other.type_name()
                )
            });

            self.0 == *val
        }
    }

    impl<A> PartialEq<Array> for ComparableDynamic<Vec<A>>
    where
        ComparableDynamic<A>: PartialEq<Dynamic>,
    {
        fn eq(&self, other: &Array) -> bool {
            &ComparableDynamic::<&[A]>(&*self.0) == other
        }
    }

    impl<const N: usize, A> PartialEq<Array> for ComparableDynamic<[A; N]>
    where
        ComparableDynamic<A>: PartialEq<Dynamic>,
    {
        fn eq(&self, other: &Array) -> bool {
            &ComparableDynamic::<&[A]>(&self.0) == other
        }
    }

    impl<A> PartialEq<Array> for ComparableDynamic<&'_ [A]>
    where
        ComparableDynamic<A>: PartialEq<Dynamic>,
    {
        fn eq(&self, other: &Array) -> bool {
            if other.len() != self.0.len() {
                return false;
            }

            other.iter().zip(self.0.iter()).all(|(other, this)| {
                // safety: repr(transparent)
                let this: &ComparableDynamic<A> = unsafe { mem::transmute(this) };
                this == other
            })
        }
    }

    use ComparableDynamic as D;
    #[allow(non_snake_case)]
    fn R<T>(val: &'static T) -> Ref<'static, T> {
        Ref::new(val, None)
    }

    test!(roundtrip: [123] => "arr" == ArrayRef::from(&[123]));
    test!(compare_array: [123] => "[1, 2, 3]" as (Array) == D([1 as INT, 2, 3]));

    tests! { (get)
        regular_borrowed: [90i32] => "arr.get_borrowed(0)" as (Ref<i32>) == R(&90),
        regular: [90] => "arr.get(0)" == 90,
        nested_borrowed: [[3]] => "arr.get_borrowed(0)" as (Ref<[i32; 1]>) == R(&[3]),
        nested_borrowed_slice: [(&[3] as &'static [_])] => "arr.get_borrowed(0)" as (Ref<&[i32]>) == R(&[3]),
        nested: [[3]] => "arr.get(0)" == [3],
        negative: [90] => "arr.get(-1)" == 90,
        negative_2: [1, 2, 3, 4] => "arr.get(-4)" == 1,
        out_of_bounds: [1, 2, 3, 4] => "arr.get(5)" == (),
        out_of_bounds_negative: [1, 2, 3, 4] => "arr.get(-5)" == (),

        #[should_panic = "ErrorFunctionNotFound"]
        non_numeric: [90] => r#"arr.get("what")"# == (),
    }

    tests! { (add)
        rhs_array_ref: [1 as INT, 2, 3] => "arr + arr" as (Array) == D([1 as INT, 2, 3, 1, 2, 3]),
        rhs_array_ref_non_int_width: [1u128, 2, 3] => "arr + arr" as (Array) == D([1u128, 2, 3, 1, 2, 3]),
        rhs_array: [1 as INT, 2, 3] => "arr + [4, 5, 6]" as (Array) == D([1 as INT, 2, 3, 4, 5, 6]),
        string_array: ["hey", "there"] => r#"arr + ["pal"]"# as (Array) == D(["hey", "there", "pal"]),
        empty_lhs: [], 'ty([(); 0]) => "arr + [1]" as (Array) == D([1 as INT]),
        empty_rhs: [1 as INT, 2] => "arr + []" as (Array) == D([1 as INT, 2]),
        mixed_types: ["heyo"] => "(arr + [1]).len()" == 2 as INT,

        #[should_panic = "ErrorFunctionNotFound"]
        invalid_rhs: [1] => "arr + 1" == (),
    }

    tests! { (equals)
        eq_empty_ref: [], 'ty([(); 0]) => "arr == arr" == true,
        eq_empty: [], 'ty([(); 0]) => "arr == []" == true,
        eq_rhs_array_ref: [1 as INT, 2, 3] => "arr == arr" == true,
        eq_rhs_array_ref_false: [1 as INT, 2, 3] => "arr == arr.split(1)" == false,
        eq_rhs_array: [1 as INT, 2, 3] => "arr == [1, 2, 3]" == true,
        eq_rhs_array_false: [1 as INT, 2, 3] => "arr == [1]" == false,
        // Note: same length so we get the type error (pass the length check).
        #[should_panic = "ErrorMismatchDataType"]
        eq_wrong_datatype: [1 as INT, 2, 3] => r#"arr == ["hey", "there", "!"]"# == (),

        ne_empty_ref: [], 'ty([(); 0]) => "arr != []" == false,
        ne_rhs_array_ref: [1 as INT, 2, 3] => "arr != arr.split(1)" == true,
        ne_rhs_array_ref_false: [1 as INT, 2, 3] => "arr != arr.split(0)" == false,
        ne_rhs_array: [1 as INT, 2, 3] => "arr != [1]" == true,
        ne_rhs_array_false: [1 as INT, 2, 3] => "arr != [1, 2, 3]" == false,
        #[should_panic = "ErrorMismatchDataType"]
        ne_wrong_datatype: [1 as INT, 2, 3] => r#"arr != ["hey", "there", "!"]"# == (),
    }

    tests! { (pop)
    }
    tests! { (shift)
    }

    tests! { (extract)
    }

    tests! { (len)
        zero_len: [(); 0] => "arr.len()" == 0 as INT,
        not_empty: [78 as INT] => "arr.len()" == 1 as INT,
        big: [(); INT::MAX as usize] => "arr.len()" == INT::MAX,
        property: [(); 40] => "arr.len" == 40 as INT,
    }
    tests! { (is_empty)
        empty: [(); 0] => "arr.is_empty()" == true,
        not_empty: [23 as INT] => "arr.is_empty()" == false,
        property: [(); INT::MAX as usize] => "arr.len" == INT::MAX,
    }

    tests! { (truncate)
    }
    tests! { (chop)
    }
    tests! { (split)
    }

    tests! { (for_each)
        // TODO: move tests up?

        // without index
        // with index
        // attempt to mutate
    }
}
