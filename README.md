# koncat
The goal of this project was to write a mid-sized haskell project in order to enhance my knowledge of the language. Hence I cannot recommend you ever using this in production as it's probably full of nasty bugs. Maybe it'll be helpful for some that try to achieve creating something similar. In that case, feel free to copy and modify (see LICENSE).
## about
The project was about creating a module execution framework for IRC bots. Basically, you put some executable somewhere in the mod/ folder which you would like to execute from IRC. Koncat will detect executables in that directory and handshake them (a special line which the executable prints to stdout which tells Koncat about its capabilities, amongst other things). Once that's happened, the module is able to be called from IRC.
Koncat also exposes an API to modules with basic IRC information (who is the user? what kind of permissions does he have?) and actions (start a dialog with user). Communication works using module stdout and stdin.

## api docs
see [haddock documentation](https://rawgit.com/talkdirty/koncat/master/dist/doc/html/koncat/index.html):
