# decumar

Decumar is a tool for interleaving latex and R. It uses specially formatted comment blocks, so your file is always valid latex.

Decumar is similar to sweave, but comes from a slightly different heritage. As well as deriving inspiration from literate programming, it also draws ideas from code generation.  Why should you use decumar instead of sweave?

 * you only need one file
 * it's always valid latex
 * caching is built in
 * you don't need to manually print ggplot2 and lattice plots
 * more types of output (figures, listing, ...)

# Basic format

Decumar blocks look like this:

    % BLOCKTYPE
    %   OPTION1: value 1
    %   OPTION2: value 2
    %
    % a <- 10
    % a + b

# latex

Decumar expects the following in your preamble:

    \usepackage{alltt}
    \usepackage{graphicx}
    \DeclareGraphicsExtensions{.png,.pdf}
    \graphicspath{{graphics-path/}}

# blocks

Decumar currently supports the following block types:

  * DEFAULTS: set up default parameters

  * CODE: execute code, but don't display it
  * LISTING: display code, but don't execute it
  * CODELISTING: execute and display code
  * INTERWEAVE: interweave each line of code with its results
  
  * GRAPHIC: insert a bare graphic
  * FIGURE: insert multiple graphics in a floating figure
  * FIGLISTING: insert a figure and the display the code used to create it
