use smallvec::SmallVec;

macro_rules! _def {
    ($($num:literal),*) => {
        $(paste::paste!{
            pub type [<SmallVec $num>]<T> = SmallVec<[T; $num]>;
        })*
    };
}

_def!(1, 2, 3, 4, 6, 8, 10, 12, 16, 20, 24, 32, 48, 64, 96, 128, 256, 512, 1024);
