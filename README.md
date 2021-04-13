# Berkeley CS61AS

This is a **WIP**

This is the homework for [CS 61AS Spring 2016](https://berkeley-cs61as.github.io/index.html). CS61AS is:

> ... a introductory computer science class at the University of California, Berkeley. We use course material derived from the classic textbook Structure and Interpretation of Computer Programs, as well as newer content developed here at Berkeley. Our primary programming languages are Racket and Scheme.

## Requirements

Instructions for an ArchLinux based system:

```sh
# Install Racket:
sudo pacman -Sy racket-minimal

# Install Berkeley libraries:
raco pkg install berkeley
```

## Run the code

Run tests for a specific homework:

```sh
racket -tm homework/grader.rkt -- homework/tests/hw0-1-tests.rkt homework/hw0-1.rkt

# or:
racket -t chatterbot/grader.rkt -- chatterbot/chatterbot.rkt
```

## License

MIT
