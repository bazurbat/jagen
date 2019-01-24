### Install Bash command autocompletion

This instruction assumes Ubuntu Linux. It will probably work just as well for
other Debian-based distributions and hopefully for others too. The way to
install the completion package will be different though.

1. Make sure you have Bash completion package installed. Use the following
   command for Ubuntu:

        sudo apt-get install bash-completion

2. Source the `<jagen_dir>/misc/bash_completion` file from the appropriate place
   in your profile. Try to add the following line to the
   `~/.config/bash_completion` or `~/.bash_completion`, create the file if it
   does not exist:

        source "<jagen_dir>/misc/bash_completion"

   Replace `<jagen_dir>` with the path to Jagen repository. This may be the
   location you've checked it out manually or inside the build root you are
   working with, something like: `~/work/root-my/.jagen/misc/bash_completion`.

3. Source your profile again, relogin or simply open a new terminal window to
   apply the changes.
