# Erlang Installation Guide using asdf

This guide covers installing Erlang using asdf version 0.18.0 on macOS, Linux, and Windows.

## Prerequisites

### MacOS
1. **Install Homebrew** (if not already installed)
```shell
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
```

2. **Install dependencies for building Erlang**
```shell
brew install autoconf openssl wxwidgets libxslt fop
brew install coreutils findutils gnu-tar gnu-sed gawk gnutls gnu-indent gnu-getopt grep ncurses libffi jq pkg-config
brew tap coin-or-tools/coinor
brew install Clp
```

### Linux (Ubuntu/Debian)

```shell
sudo apt-get update
sudo apt-get install -y build-essential autoconf m4 libncurses5-dev \
  libwxgtk3.0-gtk3-dev libwxgtk-webview3.0-gtk3-dev libgl1-mesa-dev \
  libglu1-mesa-dev libpng-dev libssh-dev unixodbc-dev xsltproc fop \
  libxml2-utils libncurses-dev openjdk-11-jdk curl git \
  coinor-clp coinor-libclp-dev libbz2-dev pkg-config libz-dev p7zip-full jq
```

### Windows

#### Using WSL2 (Recommended)
1. **Enable WSL2**
```powershell
# Run in PowerShell as Administrator
wsl --install
```

2. **Install Ubuntu from Microsoft Store**, then follow the Linux (Ubuntu/Debian) instructions above.

#### Using Git Bash/MSYS2 (Alternative)
```shell
# Install MSYS2 from https://www.msys2.org/
# Then in MSYS2 terminal:
pacman -S base-devel mingw-w64-x86_64-gcc mingw-w64-x86_64-openssl \
  mingw-w64-x86_64-ncurses git curl autoconf automake libtool \
  mingw-w64-x86_64-pkg-config bzip2 p7zip jq
```

## Installing asdf

### MacOS

```shell
# Install asdf via Homebrew
brew install asdf

# Add asdf to your shell (choose one based on your shell)

# For Zsh (add to ~/.zshrc)
echo -e "\n. $(brew --prefix asdf)/libexec/asdf.sh" >> ~/.zshrc
source ~/.zshrc

# For Bash (add to ~/.bash_profile)
echo -e "\n. $(brew --prefix asdf)/libexec/asdf.sh" >> ~/.bash_profile
source ~/.bash_profile
```

### Linux

```shell
# Clone asdf repository
git clone https://github.com/asdf-vm/asdf.git ~/.asdf --branch v0.18.0

# Add asdf to your shell (choose one based on your shell)

# For Bash (add to ~/.bashrc)
echo -e "\n. ~/.asdf/asdf.sh" >> ~/.bashrc
echo -e "\n. ~/.asdf/completions/asdf.bash" >> ~/.bashrc
source ~/.bashrc

# For Zsh (add to ~/.zshrc)
echo -e "\n. ~/.asdf/asdf.sh" >> ~/.zshrc
echo -e "\nfpath=(${ASDF_DIR}/completions \$fpath)" >> ~/.zshrc
echo -e "autoload -Uz compinit && compinit" >> ~/.zshrc
source ~/.zshrc
```

### Windows

**For WSL2:** Follow the Linux instructions above after setting up WSL2.

**For Git Bash/MSYS2:**
```shell
# Clone asdf repository
git clone https://github.com/asdf-vm/asdf.git ~/.asdf --branch v0.18.0

# Add to ~/.bashrc
echo -e "\n. ~/.asdf/asdf.sh" >> ~/.bashrc
echo -e "\n. ~/.asdf/completions/asdf.bash" >> ~/.bashrc
source ~/.bashrc
```

## Verify asdf Installation

```shell
asdf --version
# Should output: v0.18.0
```

## Installing Erlang with asdf

### 1. Add the Erlang plugin

```shell
asdf plugin add erlang https://github.com/asdf-vm/asdf-erlang.git
```

### 2. List available Erlang versions

```shell
asdf list all erlang
```

### 3. Install a specific Erlang version

```shell
# Install the latest version (as of Oct 2025)
asdf install erlang 27.1.2

# Or install a specific version
asdf install erlang 26.2.5.3
```

**Note:** Building Erlang from source can take 10-30 minutes depending on your system.

### 4. Set the Erlang version

```shell
# Set version for current directory (creates/updates .tool-versions file)
asdf set erlang 27.1.2

# Or manually create .tool-versions file
echo "erlang 27.1.2" > .tool-versions

# To set for parent directories as well (searches upwards)
asdf set -p erlang 27.1.2
```

### 5. Verify the installation

```shell
erl -version
# Should output: Erlang (SMP,ASYNC_THREADS,JIT) (BEAM) emulator version 14.2.5

# Or enter the Erlang shell
erl
# Type: q(). to exit
```

## Optional: Install Multiple Versions

asdf allows you to install and switch between multiple Erlang versions:

```shell
# Install multiple versions
asdf install erlang 27.1.2
asdf install erlang 26.2.5.3
asdf install erlang 25.3.2.14

# List installed versions
asdf list erlang

# Switch between versions by updating .tool-versions
asdf set erlang 27.1.2  # updates .tool-versions in current directory
asdf set erlang 26.2.5.3  # switch to a different version
```

## Setting Up .tool-versions File

For team projects, create a `.tool-versions` file in your project root:

```shell
# Navigate to your project directory
cd /path/to/your/project

# Create .tool-versions file
echo "erlang 27.1.2" > .tool-versions
```

Now asdf will automatically use this version when you're in this directory.

## Troubleshooting

### MacOS: OpenSSL Issues

If you encounter OpenSSL-related errors during installation:

```shell
export KERL_CONFIGURE_OPTIONS="--with-ssl=$(brew --prefix openssl@3)"
asdf install erlang 27.1.2
```

### Linux: Missing Dependencies

If build fails, ensure all dependencies are installed:

```shell
# Ubuntu/Debian
sudo apt-get install -y build-essential autoconf m4 libncurses5-dev libssl-dev
```

### Windows: WSL2 Slow Builds

If builds are slow on WSL2, ensure your project files are on the Linux filesystem (not /mnt/c/):

```shell
# Work in your Linux home directory
cd ~
mkdir projects
cd projects
```

### Build Configuration Options

You can customize the Erlang build with environment variables:

```shell
# Example: Build without Java support
export KERL_CONFIGURE_OPTIONS="--without-javac"
asdf install erlang 27.1.2

# Example: Build with specific features
export KERL_CONFIGURE_OPTIONS="--with-ssl --without-odbc --without-javac"
asdf install erlang 27.1.2
```

### Uninstalling a Version

```shell
asdf uninstall erlang 27.1.2
```

## Updating asdf

```shell
# Update asdf itself
asdf update

# Update all plugins (including erlang)
asdf plugin update --all

# Update specific plugin
asdf plugin update erlang
```

## Additional Resources

- [asdf Documentation](https://asdf-vm.com/)
- [asdf-erlang Plugin](https://github.com/asdf-vm/asdf-erlang)
- [Erlang Official Documentation](https://www.erlang.org/docs)
- [Kerl (Erlang Build Tool)](https://github.com/kerl/kerl)
