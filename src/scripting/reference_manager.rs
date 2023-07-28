use std::hash::Hash;
use std::iter::FusedIterator;
use std::mem;
use std::ops::{Deref, DerefMut};
use std::sync::Arc;

// Raw access to the internal reference inside `Ref` can cause unsafety so we
// put `Ref`'s definition in an inner module so that it's easier to verify that
// `Ref`'s accessors uphold invariants.
mod inner {
    use super::Witness;

    ////////////////////////////////////////////////////////////////////////////

    /// Wrapped reference type that allows for dynamically enforcing (i.e. at
    /// runtime) lifetime rules. See
    /// [`ReferenceManager`](super::ReferenceManager).
    ///
    /// When `Option<Witness>` is some, we must not produce references to the
    /// inner reference that aren't guarded by the [`Witness`].
    ///
    /// i.e. `&'r Ref<'l, T> -> &'q T where 'r: 'q` is okay (because an instance
    /// of `Witness` is guaranteed to live as long as the reference) but
    /// producing a bare `&'l T` is not okay (not guarded by a witness).
    /// (this is a little like `MutexGuard` and `cell::RefMut`..)
    #[derive(Debug)]
    pub struct Ref<'a, T: ?Sized = ()>(&'a T, Option<Witness>);

    // Safe: we make a new witness to guard the new reference.
    impl<'a, T: ?Sized> Clone for Ref<'a, T> {
        fn clone(&self) -> Self {
            Self(self.0.clone(), self.1.clone())
        }
    }

    impl<'a, T: ?Sized> Ref<'a, T> {
        // not unsafe/unsound, just misleading
        //
        // users will never really want to use this constructor directly; it
        // doesn't give you a reference that can be used with a `Scope` unless
        // you already have static references (in which case `Into` is good
        // enough for you)
        //
        // you also can't call this (users cannot construct witnesses)
        #[doc(hidden)]
        pub fn new(r: &'a T, witness: Option<Witness>) -> Self {
            Self(r, witness)
        }

        pub fn get_witness_copy(&self) -> Option<Witness> {
            self.1.clone()
        }

        // helper function to wrap newly created references in a new `Ref` with
        // a new witness
        pub fn mk_derived(&mut self, func: impl FnOnce(&mut Self) -> &'a T) -> Self {
            Self(func(self), self.1.clone())
        }

        // Safety: note that we're yielding a reference with lifetime `'b`, not
        // `'a`.
        #[allow(clippy::needless_lifetimes, clippy::borrow_deref_ref)]
        pub fn get<'b>(&'b self) -> &'b T {
            &*self.0
        }

        // Safety: the user can provide a difference reference for use in the
        // `Ref` (with a lifetime that "matches" under the variance of `T`).
        //
        // We have no way of checking that this reference `r` originates from
        // the same original object as `self.0` but it doesn't matter for
        // safety: we may now spuriously complain about life references to
        // `self.0`'s initial object but that's still _safe_.
        //
        // Callers should, of course, still try not to use `set` with random
        // references.
        pub fn set(&mut self, r: &'a T) {
            self.0 = r;
        }

        /// # Safety
        /// Caller must not allow this to go anywhere where it's not guarded by
        /// a witness!
        ///
        /// Unfortunately this is required for some uses that want to manipulate
        /// `self.0` (i.e. [`split_at`](slice::split_at)) and need a reference
        /// with the original lifetime to do so.
        #[allow(clippy::needless_lifetimes)]
        pub unsafe fn get_unchecked_reference(&self) -> &'a T {
            self.0
        }

        pub unsafe fn into_parts(self) -> (&'a T, Option<Witness>) {
            (self.0, self.1)
        }
    }

    // This would be unsound! Yields a bare reference not guarded by a new
    // witness and not bound to the lifetime of `self`.
    //
    // To make this safe we'd need to assert that `witness` is `None`.
    /*
    impl<T: ?Sized> Ref<'static, T> {
        pub fn get_static(&self) -> &'static T {
            self.0
        }
    }
    */

    ////////////////////////////////////////////////////////////////////////////

    /// Mutable counterpart to [`Ref`].
    #[derive(Debug)]
    pub struct RefMut<'a, T: ?Sized = ()>(&'a mut T, Option<Witness>);

    // note: no `Clone` impl (mutable references are not `Clone`!)

    impl<'a, T: ?Sized> RefMut<'a, T> {
        // see `Ref::new`
        #[doc(hidden)]
        pub fn new(r: &'a mut T, witness: Option<Witness>) -> Self {
            Self(r, witness)
        }

        pub fn get_witness_copy(&self) -> Option<Witness> {
            self.1.clone()
        }

        // see `Ref::get`
        #[allow(clippy::needless_lifetimes)]
        pub fn get<'b>(&'b self) -> &'b T {
            &*self.0
        }

        // TODO: is this okay? what happens if you do `mem::swap`?
        //
        // answer: yes; it's still fine; you can only get a reference to `&'b
        // mut T` not `&'a mut B`.
        #[allow(clippy::needless_lifetimes)]
        pub fn get_mut<'b>(&'b mut self) -> &'b mut T {
            &mut *self.0
        }

        // see `Ref::set`
        pub fn set(&mut self, r: &'a mut T) {
            self.0 = r;
        }

        // TODO: I think this is safe?
        pub fn into_shared_reference(self) -> Ref<'a, T> {
            Ref::new(&*self.0, self.1)
        }

        // varies from `Ref::get_unchecked_reference` because mutable references
        // must be unique (i.e. this consumes `self`) however the safety
        // considerations are the same
        pub unsafe fn into_parts(self) -> (&'a mut T, Option<Witness>) {
            (self.0, self.1)
        }
    }

    ////////////////////////////////////////////////////////////////////////////

    /// An owned value that contains borrowed data whose lifetime we want to
    /// dynamically enforce.
    ///
    /// The safety property here is that we cannot allow the value in `self.0`
    /// to "escape" (i.e. not be guarded by a witness).
    ///
    /// this is.. unfortunately virtually impossible to enforce while allowing
    /// any kind of access to the underlying type. Consider a type like
    /// [`std::slice::Iter<'_>`] that has [`std::slice::Iter<'_, T>::as_slice`].
    /// This method yields data from the inner reference such that it is no
    /// longer tracked by a witness!
    ///
    /// so this type is essentially opaque.
    #[derive(Debug)]
    pub struct Owned<T>(T, Option<Witness>);

    // safety: we're making a new witness
    impl<T: Clone> Clone for Owned<T> {
        fn clone(&self) -> Self {
            Self(self.0.clone(), self.1.clone())
        }
    }

    #[allow(clippy::needless_lifetimes)]
    impl<T> Owned<T> {
        pub fn new(val: T, witness: Option<Witness>) -> Self {
            Self(val, witness)
        }

        pub fn get_witness_copy(&self) -> Option<Witness> {
            self.1.clone()
        }

        // these are unsafe because you can then use the reference to `T` to
        // invoke methods that yield references to inner borrowed data...
        //
        // when using these you have to promise that you have ensured no inner
        // borrowed data can escape
        pub unsafe fn get<'b>(&'b self) -> &'b T {
            &self.0
        }
        pub unsafe fn get_mut<'b>(&'b mut self) -> &'b mut T {
            &mut self.0
        }

        // safe
        pub fn set(&mut self, r: T) {
            self.0 = r;
        }

        pub unsafe fn into_parts(self) -> (T, Option<Witness>) {
            (self.0, self.1)
        }
    }
}

pub use inner::{Owned, Ref, RefMut};

////////////////////////////////////////////////////////////////////////////////

/* Ref impls: */

impl<T: ?Sized> Deref for Ref<'_, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.get()
    }
}

impl<'a, 'b, T: ?Sized + PartialEq<R>, R: ?Sized> PartialEq<Ref<'b, R>> for Ref<'a, T> {
    fn eq(&self, other: &Ref<'b, R>) -> bool {
        T::eq(self.get(), other.get())
    }
}
impl<'a, 'b, T: ?Sized + PartialEq<R>, R: ?Sized> PartialEq<RefMut<'b, R>> for Ref<'a, T> {
    fn eq(&self, other: &RefMut<'b, R>) -> bool {
        T::eq(self.get(), other.get())
    }
}

/*
impl<'a, T: ?Sized + PartialEq<R>, R> PartialEq<Owned<R>> for Ref<'a, T> {
    fn eq(&self, other: &Owned<R>) -> bool {
        // cannot be sure that `T::eq` will not let references to inner borrowed
        // data escape...
        T::eq(self.get(), unsafe { other.get() })
    }
}
*/
// Cannot offer, unfortunately:
/*
impl<'a, T: ?Sized + PartialEq<R>, R: ?Sized> PartialEq<T> for Ref<'a, T> {
    fn eq(&self, other: &T) -> bool {
        T::eq(self.get(), other)
    }
}
*/

impl<'a, T: ?Sized + Eq> Eq for Ref<'a, T> {}

impl<'a, T: ?Sized + Hash> Hash for Ref<'a, T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        T::hash(self.get(), state)
    }
}

// safety: not unsafe to construct but not particularly useful...
impl<'a, T: ?Sized> From<&'a T> for Ref<'a, T> {
    fn from(value: &'a T) -> Self {
        Ref::new(value, None)
    }
}

// safety: same as above
impl<'r, const N: usize, T> From<&'r [T; N]> for Ref<'r, [T]> {
    fn from(value: &'r [T; N]) -> Self {
        Ref::new(value, None)
    }
}

// we want to support specifically the `IntoIterator` impls that only actually
// need a _reference_ to `self` and do not need to actually consume `self`...
//
// for now we ask for this by doing a blanket impl on `&'a T: IntoIterator`
impl<'a, T: ?Sized> IntoIterator for Ref<'a, T>
where
    &'a T: IntoIterator,
{
    // NOTE: This may actually be unsound since `IntoIter::next` can yield data
    // from `'a` that's not guarded by a witness! Once we produce the `IntoIter`
    // type, even though it is guarded by a reference, nothing prevents it from
    // yielding data from an inner shared reference...
    //
    // To make it sound we have to wrap yielded values in a `Owned`.
    //
    // For now we lean on `Owned` to this for us (note that it's IntoIterator
    // impl is technically unsound in the general case).
    type Item = Owned<<&'a T as IntoIterator>::Item>;
    // We're assuming that `IntoIter` contains borrowed data with lifetime `'a`.
    type IntoIter = Owned<<&'a T as IntoIterator>::IntoIter>;

    fn into_iter(self) -> Self::IntoIter {
        // Safety: see above. not perfect but we're wrapping this in `Owned`
        // so it's opaque..
        let (r, witness) = unsafe { self.into_parts() };
        Owned::new(r.into_iter(), witness)
    }
}

////////////////////////////////////////////////////////////////////////////////

/* RefMut impls: */

impl<T: ?Sized> Deref for RefMut<'_, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.get()
    }
}

impl<T: ?Sized> DerefMut for RefMut<'_, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.get_mut()
    }
}

impl<'a, 'b, T: ?Sized + PartialEq<R>, R: ?Sized> PartialEq<RefMut<'b, R>> for RefMut<'a, T> {
    fn eq(&self, other: &RefMut<'b, R>) -> bool {
        T::eq(self.get(), other.get())
    }
}

impl<'a, 'b, T: ?Sized + PartialEq<R>, R: ?Sized> PartialEq<Ref<'b, R>> for RefMut<'a, T> {
    fn eq(&self, other: &Ref<'b, R>) -> bool {
        T::eq(self.get(), other.get())
    }
}

/*
// cannot be sure that `T::eq` will not let references to inner borrowed data
// escape...

impl<'a, T: ?Sized + PartialEq<R>, R> PartialEq<Owned<R>> for RefMut<'a, T> {
    fn eq(&self, other: &Owned<R>) -> bool {
        T::eq(self.get(), other.get())
    }
}
*/

impl<'a, T: ?Sized + Eq> Eq for RefMut<'a, T> {}

impl<'a, T: ?Sized + Hash> Hash for RefMut<'a, T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        T::hash(self.get(), state)
    }
}

impl<'a, T: ?Sized> From<&'a mut T> for RefMut<'a, T> {
    fn from(value: &'a mut T) -> Self {
        RefMut::new(value, None)
    }
}

impl<'r, const N: usize, T> From<&'r mut [T; N]> for RefMut<'r, [T]> {
    fn from(value: &'r mut [T; N]) -> Self {
        RefMut::new(value, None)
    }
}

// Note: cannot implement IntoIterator due to coherence :/

impl<'a, T: ?Sized + Iterator> Iterator for RefMut<'a, T> {
    // re: whether we need to guard yielded values here with a witness:
    //
    // The thinking is that yielded values may refer to borrowed data (i.e. if
    // `T` is not `'static`) and that that borrowed data would not be tracked by
    // a witness.
    //
    // However, it's worth noting that we do not artifially extend any lifetimes
    // _within_ `T`; just `&'a T`. So I think we're actually fine.
    //
    // It's types like `slice::Iter<'a>` that _can_ yield borrowed data and it's
    // up to the methods that construct an `Iter<'static>` from an artificially
    // lifetime extended `Ref` to make sure that the `Iter<'static>` is then
    // appropriately wrapped so that it only yields witness-guarded references.
    type Item = <T as Iterator>::Item;

    fn next(&mut self) -> Option<Self::Item> {
        self.get_mut().next()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.get().size_hint()
    }
}
impl<'a, T: ?Sized + ExactSizeIterator> ExactSizeIterator for RefMut<'a, T> {}
impl<'a, T: ?Sized + FusedIterator> FusedIterator for RefMut<'a, T> {}
impl<'a, T: ?Sized + DoubleEndedIterator> DoubleEndedIterator for RefMut<'a, T> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.get_mut().next_back()
    }
}

////////////////////////////////////////////////////////////////////////////////

/* Owned impls: */

// all unsafe; cannot be sure that references used will not be used to leak
// inner references
//
// going to lean on our `FullyOwnedType` thing for now but this is not great..
/**/
impl<T: FullyOwnedType> Deref for Owned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { self.get() }
    }
}

impl<T: FullyOwnedType + PartialEq<R>, R: FullyOwnedType> PartialEq<Owned<R>> for Owned<T> {
    fn eq(&self, other: &Owned<R>) -> bool {
        T::eq(unsafe { self.get() }, unsafe { other.get() })
    }
}
impl<'a, T: FullyOwnedType + PartialEq<R>, R: FullyOwnedType + ?Sized> PartialEq<Ref<'a, R>>
    for Owned<T>
{
    fn eq(&self, other: &Ref<'a, R>) -> bool {
        T::eq(unsafe { self.get() }, other.get())
    }
}
impl<'a, T: FullyOwnedType + PartialEq<R>, R: FullyOwnedType + ?Sized> PartialEq<RefMut<'a, R>>
    for Owned<T>
{
    fn eq(&self, other: &RefMut<'a, R>) -> bool {
        T::eq(unsafe { self.get() }, other.get())
    }
}

impl<T: FullyOwnedType + Eq> Eq for Owned<T> {}

impl<T: FullyOwnedType + Hash> Hash for Owned<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        T::hash(unsafe { self.get() }, state)
    }
}
/**/

impl<T> From<T> for Owned<T> {
    fn from(value: T) -> Self {
        Owned::new(value, None)
    }
}

impl<T: Iterator> Iterator for Owned<T> {
    type Item = Owned<<T as Iterator>::Item>;

    // this is still unsound since technically the inner implementation of
    // `Iterator` can go and hide state in other places but... we're going to
    // tolerate it for now (TODO: unsafe)
    fn next(&mut self) -> Option<Self::Item> {
        let next = unsafe { self.get_mut() }.next();
        next.map(|x| Owned::new(x, self.get_witness_copy()))
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        unsafe { self.get() }.size_hint()
    }
}
impl<T: ExactSizeIterator> ExactSizeIterator for Owned<T> {}
impl<T: FusedIterator> FusedIterator for Owned<T> {}
impl<T: DoubleEndedIterator> DoubleEndedIterator for Owned<T> {
    fn next_back(&mut self) -> Option<Self::Item> {
        // TODO: unsafe
        let next = unsafe { self.get_mut() }.next_back();
        next.map(|x| Owned::new(x, self.get_witness_copy()))
    }
}

////////////////////////////////////////////////////////////////////////////////

#[doc(hidden)]
pub trait FullyOwnedType: 'static + sealed::Private {}
impl<T: sealed::Private> FullyOwnedType for T {}

mod sealed {
    pub trait Private: 'static {}
}

// escape hatch for `Owned` (aka `Opaque`) for types that definitely do not
// borrow any external data that they don't manage themselves
//
// i.e. `Cow<'static, str>` is _not_ one of these types because our
// `ReferenceManager` goop could technically produce one? ... (not sure)
macro_rules! fully_owned_types {
    ($($ty:ty $(,)?)*) => {
        $(
            impl sealed::Private for $ty { }
        )*
    };
}

macro_rules! fully_owned_collections {
    (
        // `ty` as `tt` is a bad approximation but w/e
        $($ty:tt[$($generics:ident),*]),* $(,)?
    ) => {
        $(
            impl<$($generics: FullyOwnedType),*> sealed::Private for $ty<$($generics),*> { }
        )*
    };
}

macro_rules! fully_owned_tuples {
    // triangle is right aligned instead of left aligned but whatever
    ($first:ident $($ident:ident)*) => {
        impl<$first: FullyOwnedType, $($ident: FullyOwnedType),*> sealed::Private for ($first, $($ident),*) { }
        fully_owned_tuples!( $($ident)*);
    };
    () => {}
}

mod exemptions {
    use super::{sealed, FullyOwnedType};
    use std::{
        cell::{Cell, RefCell},
        collections::{BTreeMap, BTreeSet, BinaryHeap, HashMap, HashSet, LinkedList},
        ffi::{OsStr, OsString},
        path::{Path, PathBuf},
        rc::Rc,
        sync::{Arc, Mutex, RwLock},
    };

    fully_owned_types! {
        ()
        u8 u16 u32 u64 u128 usize
        i8 i16 i32 i64 i128 isize
        bool char
        f32 f64

        &'static str, String
        &'static OsStr, OsString
        &'static Path, PathBuf
    }

    // TODO: should be generic over the allocator...
    fully_owned_collections! {
        Option[T], Result[T, E], Box[T],

        Vec[T], HashMap[K, V], HashSet[T], BTreeSet[T], BTreeMap[K, V],
        LinkedList[T], BinaryHeap[T],

        Cell[T], RefCell[T], Rc[T], Arc[T], Mutex[T], RwLock[T],
    }

    fully_owned_tuples!( A B C D E F G H I J K );

    impl<T: FullyOwnedType> sealed::Private for *const T {}
    impl<T: FullyOwnedType> sealed::Private for *mut T {}

    impl<const N: usize, T: FullyOwnedType> sealed::Private for [T; N] {}
}

impl<T: FullyOwnedType> Owned<T> {
    pub fn to_inner(self) -> T {
        unsafe { self.into_parts() }.0
    }
}

////////////////////////////////////////////////////////////////////////////////

use std::{marker::PhantomData, sync::Mutex};

/// 'env: surrounding lifetime that we can borrow from
/// 'scope: lifetime of the scope
pub struct ReferenceManager<'scope, 'env: 'scope> {
    // to keep `'scope` from shrinking; see `std::thread::Scope`
    scope: PhantomData<&'scope mut &'scope ()>,
    env: PhantomData<&'env mut &'env ()>,

    table: Mutex<Vec<(Witness, Option<String>)>>,
}

// TODO: do we actually need `'env`?
// I think it's just there for lifetime variance reasons, maybe?
//  - nah, it's associated with the function I think

impl ReferenceManager<'static, 'static> {
    pub fn scope<'env, R>(
        func: impl for<'scope> FnOnce(&'scope ReferenceManager<'scope, 'env>) -> R,
    ) -> R {
        let refm = ReferenceManager {
            scope: PhantomData,
            env: PhantomData,
            table: Mutex::new(Vec::new()),
        };

        // TODO: panic safety?

        let ret = func(&refm);

        let table = refm.table.lock().unwrap();
        for (witness, name) in table.iter() {
            let strong_count = Arc::strong_count(&witness.0);
            if strong_count > 1 {
                panic!("Error: {} still has {} live reference(s) at the end of the reference manager's scope!", name.as_deref().unwrap_or("an allocation"), strong_count - 1);
            }
        }

        ret
    }
}

impl<'s, 'e> ReferenceManager<'s, 'e> {
    pub fn make_checked_reference<'r: 'e, T: ?Sized>(
        &'s self,
        reference: &'r T,
        name: Option<impl ToString>,
    ) -> Ref<'static, T>
    where
        Ref<'r, T>: From<&'r T>,
    {
        let witness = Witness(Arc::new(()));
        let mut table = self.table.lock().unwrap();
        table.push((witness.clone(), name.map(|x| x.to_string())));

        let reference = Ref::new(reference, Some(witness));

        // ...
        // this may very well be unsafe in the presence of invariant lifetimes?
        //
        // not sure
        unsafe { mem::transmute(reference) }
    }

    pub fn make_checked_mut_reference<'r: 'e, T: ?Sized>(
        &'s self,
        reference: &'r mut T,
        name: Option<impl ToString>,
    ) -> RefMut<'static, T>
    where
        RefMut<'static, T>: From<&'r mut T>,
    {
        let witness = Witness(Arc::new(()));
        let mut table = self.table.lock().unwrap();
        table.push((witness.clone(), name.map(|x| x.to_string())));

        let reference = RefMut::new(reference, Some(witness));

        // ...
        // this may very well be unsafe in the presence of invariant lifetimes?
        //
        // not sure
        unsafe { mem::transmute(reference) }
    }
}

#[derive(Debug, Clone)]
pub struct Witness(Arc<()>);

// safety property is that you must store the witness somewhere..
//
// and must not allow users to get the bare inner reference
// pub unsafe trait MakeCheckedReference {
//     fn new<'r, T: ?Sized>(reference: &'r T, witness: Witness) -> Ref<'r, T>;
// }

// impl<'a, T: Clone> IntoIterator for ArrayRef<'a, T> {
//     type Item = T;
//     type IntoIter = std::iter::Cloned<std::slice::Iter<'a, T>>;
//
//     fn into_iter(self) -> Self::IntoIter {
//         self.0.iter().cloned()
//     }
// }

// TODO: what to do about lifetimes..
//  - lets do a "watched lifetime" kind of thing
//    + you set up an outer scope that bounds the lifetime
//    + you hand that scope a closure, we'll give you a token with the lifetime
//      in it
//    + you can use the token to turn things with that lifetime into 'static
//      references
//    + we'll keep track of what references are still alive via a refcount
//    + when the scope exits if there are any extant references we'll panic

// TODO: okay yeah, need a `Ref` wrapper type..
/*
impl<'a, T> IntoIterator for ArrayRef<'a, T> {
    type Item = &'a T;
    type IntoIter = std::slice::Iter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}
*/

#[cfg(test)]
mod tests {
    use super::super::ArrayRef;

    use super::*;

    use rhai::{Dynamic, INT};

    #[test]
    #[should_panic = "array still has 1 live reference"]
    fn test_reference_guard_simple() {
        let array = [1, 2, 3, 4, 5];
        let mut yo = None;

        ReferenceManager::scope(|r| {
            let array_ref: ArrayRef<'static, _> = r.make_checked_reference(&array, Some("array"));

            yo = Some(array_ref.clone());
        });
    }

    #[test]
    fn test_reference_guard() {
        let mut scope = rhai::Scope::new();
        scope.push_dynamic("array", Dynamic::UNIT);
        scope.push_dynamic("array2", Dynamic::UNIT);

        let mut engine = rhai::Engine::new();
        engine.build_type::<ArrayRef<'static, INT>>();
        engine.build_type::<ArrayRef<'static, ()>>();

        let array = [1, 2, 3, 4, 5];
        let array2 = [(); 40];

        ReferenceManager::scope(|r| {
            let array_ref: ArrayRef<'static, _> = r.make_checked_reference(&array, Some("array"));
            let array_ref2: ArrayRef<'static, _> =
                r.make_checked_reference(&array2, Some("array2"));

            // under real use we'd probably clone the scope or roll it back in a
            // more rigorous way
            scope.push("a", array_ref);
            scope.push("b", array_ref2);

            engine
                .run_with_scope(
                    &mut scope,
                    "
                array = a;
                array2 = b;

                let foo = a;
                let bar = a;
                let baz = a;
            ",
                )
                .unwrap();

            // 5 live references for a

            // scope.remove::<ArrayRef<'static, INT>>("a").unwrap();
            let _ = scope.remove::<Dynamic>("a").unwrap();
            let _ = scope.remove::<Dynamic>("array").unwrap();
            let _ = scope.remove::<Dynamic>("foo").unwrap();
            let _ = scope.remove::<Dynamic>("bar").unwrap();
            let _ = scope.remove::<Dynamic>("baz").unwrap();

            let _ = scope.remove::<Dynamic>("b").unwrap();
            let _ = scope.remove::<Dynamic>("array2").unwrap();

            // TODO: we can actually go hunting through the scope for stuff, programmatically..
            // types are tricky though

            // scope.remove::<ArrayRef<'static, INT>>("foo").unwrap();
            // scope.remove::<ArrayRef<'static, INT>>("bar").unwrap();
        });
    }
}
