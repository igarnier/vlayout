# vlayout
Declarative interface to Cairo

TODOs:
Add proper transformation system for commands
Add ellipses

BUGS:
in Commands.scale, we do not properly transform circles (since we don't have ellipses)
Text is not handled in a very nice way. Find how to evaluate the bbox of a chunk of text
as a function of the font, text size and etc.