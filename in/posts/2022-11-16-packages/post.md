--------------------------------------------------------------------------------
title: The value of packaging
date: 2022 Nov 16
slug: packaging
abstract: How publishing packages makes programming tractable
--------------------------------------------------------------------------------

Michael Feathers defined "legacy" code as anything without tests. I think of it
more generally as code that hasn't been packaged, where "packaging" can entail a
number of things, including tests, but also any work one might do before feeling
good about publishing a library.

The best tool I have seen to avoid the codebase from becoming an accumulation of
mush is for there not to be "a codebase", or at least for it to be as small as
possible, achieved by continually getting stuff out of it and into the
boundaries of versioned packages.

Nothing is, of course, "the" code -- That is, one is never referring to all the
code in the world, all the code an organization has ever written, or even all
the code that is involved in building a particular executable. There is always
some particular scope implicit in the "the". So what is it? "Codebase" is often
synonymous with "legacy". It is everything within the locus of our concern.
Notably, this generally excludes third-party packages.

One way to help put code out of mind is by getting it under test. But this is
only a half measure.

It seems to be very easy to end up as a working programmer with absolutely no
experience with constructing package boundaries. In school, the largest project
scale I ever saw was semester-long, and even in "software engineering" class I
was not instructed or incentivized to create multiple meaningfully distinct
components. The semester's work was in *the codebase* for the semester.

The experience of pulling a chunk of code out of the application and into a
package meant for public consumption is eye-opening. With this shift in aim,
suddenly I am looking at everything in a new light and I see every opportunity
to polish. Maybe a few things need to be added to make a module comprehensive,
maybe some things need to be organized or made more consistent, documentation
written and clarified, and, yes, tests added. And I realize that this piece of
the project, which I had thought of as good enough for myself but not good
enough to publish, was really not good enough for myself either.

Legacy code is that which has not undergone a finishing step: a brief time when
quality was the author's chief concern, over a unit of code small enough to
think about comprehensively, with a sincere intent to then leave it alone for a
while. It is essential to continually be carving these pieces out of the
codebase, the place where nothing is safe from the jostling of everyday work.
