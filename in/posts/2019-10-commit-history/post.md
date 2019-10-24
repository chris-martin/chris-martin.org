--------------------------------------------------------------------------------
title:    Commit history
date:     2019 Oct 24
slug:     commit-history
abstract: Keep it how you like.
--------------------------------------------------------------------------------

"Merge or rebase?" seems to be a perpetual source of debate. It shouldn't be; don't listen to anybody with an unconditional opinion on this topic. Whether it's worth maintaining a cleaned-up version history depends entirely on the kind of project!

Some reasons to rebase commits to clean up the log:

  - With a slow-moving open source project where the commit log is something that you expect a lot of people will read to understand the changes, you might want to squash and rebase development branches to present a simple log where each commit represents the sort of item that you might list in a change log.
  - At my first job when we had a bunch of student employees and no pre-merge code review process yet, there were some people who would read over the commit log every morning. That's an example where you might want people to consolidate their commits.

If you're not in the kind of situation where the log itself is a product, then micromanaging the git log probably isn't worth the time cost and potential loss of information that can occur in a rebase. For our code that runs [Type Classes](https://typeclasses.com), there's just very little point in caring what the history looks like. There's nobody to look at it but Julie and me, and once a revision is deployed, the previous commits are pretty much irrelevant. It's good to have the history just-in-case, but since it's uncommon for us to look at the history, it's not worth expending any effort to make that easier.

Software is a tremendously wide umbrella covering a huge ranges of activities, and generalizations about it tend to be absurd. It's easy to get immersed in your own particular office or project and feel like your experience there is the universal experience of programming. And when we make that mistake and also have loud opinions, we alienate others for no good reason.

Keep this in mind when you read opinions, even from people whose knowledge and experience you respect.

And keep this in mind when you express opinions:

  - Am I claiming something more broad than my knowledge?
  - Am I writing "you" mean I actually mean "me"?
  - Would including some extra details help limit the scope of my anecdotes ("I work at a mid-size company", "I work on open source projects", "the products I've worked on have all been videogames", "I use javascript", "I don't have children", etc.) to avoid accidentally implying an excessive generalization?
