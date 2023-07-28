use crate::{ Capability, Read, Write, Copy, Move, Delete, NotGranted };

use std::marker::PhantomData;
use std::{ path, fs, io };

#[derive(Clone, Debug)]
pub struct RE;
pub struct WR;
pub struct AP;
pub struct TR;
pub struct CR;
pub struct CRN;

pub struct OpenOptions<A, B, C, D, E, F>(&mut fs::OpenOptions, PhantomData::<(A, B, C, D, E, F)>);

pub trait OpenOptionsTrait {}
impl<A, B, C, D, E, F> OpenOptionsTrait for OpenOptions<A, B, C, D, E, F> {}

impl OpenOptions<(), (), (), (), (), ()> {
    pub fn new() -> Self {
        OpenOptions(&mut fs::OpenOptions::new(), PhantomData::<((), (), (), (), (), ())>)
    }
}

impl<A, B, C, D, E, F> OpenOptions<A, B, C, D, E, F> {
    pub fn read(&mut self, read: bool) -> Box<dyn OpenOptionsTrait> {
        if read {
            return Box::new(
                OpenOptions::<RE, B, C, D, E, F>(
                    self.0.read(true),
                    PhantomData::<(RE, B, C, D, E, F)>
                )
            );
        }

        return Box::new(
            OpenOptions::<(), B, C, D, E, F>(
                self.0.read(false),
                PhantomData::<((), B, C, D, E, F)>
            )
        );
    }

    pub fn write(&mut self, write: bool) -> &mut Self {
        self.0.write(write);
        self
    }

    pub fn append(&mut self, append: bool) -> &mut Self {
        self.0.append(append);
        self
    }

    pub fn truncate(&mut self, truncate: bool) -> &mut Self {
        self.0.truncate(truncate);
        self
    }

    pub fn create(&mut self, create: bool) -> &mut Self {
        self.0.create(create);
        self
    }

    pub fn create_new(&mut self, create_new: bool) -> &mut Self {
        self.0.create_new(create_new);
        self
    }

    pub fn open<G, H, I, J, K>(&self, cap: &Capability<G, H, I, J, K>) -> io::Result<fs::File> {
        self.0.open(cap.get_path())
    }
}

pub fn canonicalize<A, B, C, D, E>
(cap: &Capability<A, B, C, D, E>) -> io::Result<path::PathBuf> {
    fs::canonicalize(cap.get_path())
}

pub fn copy<A, B, C, D, E, F, G, H>
(from: &Capability<A, B, Copy, C, D>, to: &Capability<E, Write, F, G, H>) -> io::Result<u64> {
    fs::copy(from.get_path(), to.get_path())
}

pub fn create_dir<A, B, C, D>
(cap: &Capability<A, Write, B, C, D>) -> io::Result<()> {
    fs::create_dir(cap.get_path())
}

pub fn create_dir_all<A, B, C, D, E, F, G, H>
(cap: &Capability<A, Write, B, C, D>) -> io::Result<()> {
    fs::create_dir_all(cap.get_path())
}

pub fn hard_link<A, B, C, D, E, F, G, H>
(original: &Capability<Read, A, B, C, D>, link: &Capability<E, Write, F, G, H>) -> io::Result<()> {
    panic!("[Coenobita] [ERROR] Hard linking is not properly supported yet.");
}

pub fn metadata<A, B, C, D>
(cap: &Capability<Read, A, B, C, D>) -> io::Result<fs::Metadata> {
    fs::metadata(cap.get_path())
}

pub fn read<A, B, C, D>
(cap: &Capability<Read, A, B, C, D>) -> io::Result<Vec<u8>> {
    fs::read(cap.get_path())
}

pub fn read_dir<A, B, C, D>
(cap: &Capability<Read, A, B, C, D>) -> io::Result<fs::ReadDir> {
    fs::read_dir(cap.get_path())
}

pub fn read_link<A, B, C, D>
(cap: &Capability<Read, A, B, C, D>) -> io::Result<path::PathBuf> {
    fs::read_link(cap.get_path())
}

pub fn read_to_string<A, B, C, D>
(cap: &Capability<Read, A, B, C, D>) -> io::Result<String> {
    fs::read_to_string(cap.get_path())
}

pub fn remove_dir<A, B, C, D>
(cap: &Capability<A, B, C, D, Delete>) -> io::Result<()> {
    fs::remove_dir(cap.get_path())
}

pub fn remove_dir_all<A, B, C, D>
(cap: &Capability<A, B, C, D, Delete>) -> io::Result<()> {
    fs::remove_dir_all(cap.get_path())
}

pub fn remove_file<A, B, C, D>
(cap: &Capability<A, B, C, D, Delete>) -> io::Result<()> {
    fs::remove_file(cap.get_path())
}

pub fn rename<A, B, C, D, E, F, G, H>
(from: &Capability<A, B, C, Move, D>, to: &Capability<E, F, G, Move, H>) -> io::Result<()> {
    fs::rename(from.get_path(), to.get_path())
}

pub fn set_permissions
(cap: &Capability<Read, Write, Copy, Move, Delete>, perm: fs::Permissions) -> io::Result<()> {
    fs::set_permissions(cap.get_path(), perm)
}

pub fn symlink_metadata<A, B, C, D>
(cap: &Capability<Read, A, B, C, D>) -> io::Result<fs::Metadata> {
    fs::symlink_metadata(cap.get_path())
}

pub fn write<A, B, C, D, E: AsRef<[u8]>>
(cap: &Capability<A, Write, B, C, D>, contents: E) -> io::Result<()> {
    fs::write(cap.get_path(), contents)
}
