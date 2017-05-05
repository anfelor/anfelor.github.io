# Anton Lorenzen's blog

This is the code for the blog at [https://anfelor.github.io/blog/](https://anfelor.github.io/blog/).

## Structure

In the `src/Entries` file you will find the blog posts, written in a template haskell dsl.
The other files in `src/` deal with transforming these posts into the html in `blog/`.

## Workflow

Install the [steeloverseer](https://github.com/schell/steeloverseer) and execute `sos` on the command line.
This will trigger a `stack build` and `stack exec blog` in which the posts, `.scss` and `.ts` files are compiled and 
the `blog/` directory overwritten with the result.

**DANGER!** Every rebuild overwrites the `blog/` directory! Never modify/store anything in there!
