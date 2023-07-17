mod internal {
    use crate::{ Capability, Read, Write, Copy, Move, Delete, NotGranted };
    use std::{ path, fs, io };

    pub struct _OpenOptions;

    pub trait _OpenOptionsOverload<A, B, C, D, E> {
        fn open(&self, cap: &Capability<A, B, C, D, E>) -> io::Result<fs::File>;
    }

    impl _OpenOptionsOverload<Read, NotGranted, NotGranted, NotGranted, NotGranted> for _OpenOptions {
        fn open
        (&self, cap: &Capability<Read, NotGranted, NotGranted, NotGranted, NotGranted>) -> io::Result<fs::File> {
            fs::OpenOptions::new()
                .read(true)
                .open(cap.get_path())
        }
    }

    impl _OpenOptionsOverload<Read, Write, NotGranted, NotGranted, NotGranted> for _OpenOptions {
        fn open(&self, cap: &Capability<Read, Write, NotGranted, NotGranted, NotGranted>) -> io::Result<fs::File> {
            fs::OpenOptions::new()
                .read(true)
                .write(true)
                .open(cap.get_path())
        }
    }
}

pub struct OpenOptions;

impl OpenOptions {
    pub fn open<A, B, C, D, E>
    (cap: &Capability<A, B, C, D, E>) -> io::Result<fs::File>
    where internal::_OpenOptions: internal::_OpenOptionsOverload<A, B, C, D, E> {
        internal::_OpenOptionsOverload::open(&internal::_OpenOptions, cap)
    }
}

// Alternative exploration - doesn't work properly
#[derive(Debug)]
pub struct OpenOptions<A, B, C> {
    phantom: PhantomData<(A, B, C)>
}

impl OpenOptions<(), (), ()> {
    pub fn new() -> OpenOptions<NotGranted, NotGranted, NotGranted> {
        OpenOptions::<NotGranted, NotGranted, NotGranted> {
            phantom: PhantomData::<(NotGranted, NotGranted, NotGranted)>
        }
    }
}

impl<A, B, C> OpenOptions<A, B, C> {
    pub fn read(&self, set: bool) -> OpenOptions<A, B, C> {
        OpenOptions::<Read, B, C> {
            phantom: PhantomData::<(Read, B, C)>
        }
    }

    pub fn write(&self, set: bool) -> OpenOptions<A, Write, C> {
        OpenOptions::<A, Write, C> {
            phantom: PhantomData::<(A, Write, C)>
        }
    }
}

pub trait Open<A, B, C, D, E> {
    fn open(cap: &Capability<A, B, C, D, E>) -> io::Result<fs::File>;
}

impl<A, B, C, D> Open<Read, A, B, C, D> for OpenOptions<Read, NotGranted, NotGranted> {
    fn open(cap: &Capability<Read, A, B, C, D>) -> io::Result<fs::File> {
        fs::OpenOptions::new()
            .read(true)
            .open(cap.get_path())
    }
}
