--------------------------------------------------------------------------------
title:    The app-concepts diamond
date:     2019 May 25
slug:     app-concepts-diamond
abstract: An observation about a typical application's dependency graph.
--------------------------------------------------------------------------------

I find that when I break up an application into packages by feature, the dependency graph nearly always has this "diamond" shape:

<div style="text-align: center;"><img src="${diamond.png}" style="max-width: 100%;"></div>

Before I realized this, I struggled a lot. My naive impulse was to have four packages: "Feature 1", "Feature 2", "Feature 3", and a "Common" package for non-feature-specific stuff. This leads to one of three huge mistakes:

  1. Dependency cycles (each feature imports the common project, and the common project imports the features);

  2. Not having the "app concepts" component (this is usually only possible in untyped languages);

  3. Not having the "app" component (its role is instead fulfilled by configuration for some framework - Java Servlets, cgi-bin, BEAM, what have you).

I get tempted by the second and third mistakes because both the "app concepts" and "app" components are often very small. Sometimes the "app concepts" package is only [a handful of newtypes](https://hackage.haskell.org/package/stripe-concepts). Sometimes all the "app" package does is import the features and put them into a list. But even if a package only contains a single thing, if that thing is necessary, then that thing needs a place.
