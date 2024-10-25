# Coenobita

## Getting Started
First, you will need to install Coenobita on your machine.

### Installing Coenobita
#### Manual
1. Clone this repository.
2. Enter the root directory and run `./install.sh`.

### Using Coenobita
1. If you haven't done so already, create a file called `.cargo/config.toml` in the root directory of your project. It should look something like this.

```toml
[build]
rustc-wrapper = "coenobita"
```