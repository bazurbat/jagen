### Install Bash command autocompletion

1. Make sure you have Bash completion package installed.

   Use the following command on Ubuntu/Debian Linux:

        sudo apt-get install bash-completion

2. Source the included `bash_completion` file in your profile.

   Add the following line:

       source "<jagen_dir>/misc/bash_completion"

   either to:
      
   - `~/.bash_completion` file, which should work also on macOS with an older
     completion package for Bash 3.2
   - `~/.local/share/bash-completion/completions/jagen.bash` file to autoload
     the completions as needed, which is a feature of a newer Bash completion
     version

   Replace the `<jagen_dir>` with the path to the Jagen repository.

3. Source your profile again, relogin or open a new terminal window to apply
   the changes.
