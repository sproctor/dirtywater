id = 1001
title = "Starting Room"
description = "...with a simple description"
north = 1002

or description could be like:
description = function (c) return "you are not welcome here, " .. c.name end

We can check whether it's a function or not when we read the file and pass in the character when we lookup the description
