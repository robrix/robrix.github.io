---
layout: post
title:  "On the Order of Neptune"
date:   2014-04-20 04:20:00
categories: code
---

[Inscribe][] the [orbit of Neptune][] in a square.

<!--more-->

Now, take a pair of integers as _x_ and _y_ coordinates across this square. Their size in bits determines the resolution at which they can measure this square.

An integer of _n_ bits can hold any of 2*ⁿ* distinct values. 32-bit integers, therefore, would divide the square into a grid of 2³² points.

At 32 bits of resolution, adjacent coordinates, e.g. …<code>0101</code> and …<code>0110</code>, are about a kilometre apart on our square.

If we double the size of our integers, we now divide the square into a grid of 2⁶⁴ points.

At 64 bits of resolution, still covering the _entire span of the orbit of Neptune_, adjacent coordinates are about 0.24µm apart, or about 1% of the width of [an average human hair][].

And famously, populating a 128-bit address space would require us to [boil the oceans][].

[Inscribe]: http://en.wikipedia.org/wiki/Inscribed_figure
[orbit of Neptune]: http://en.wikipedia.org/wiki/Neptune#Orbit_and_rotation
[an average human hair]: http://hypertextbook.com/facts/1999/BrianLey.shtml
[boil the oceans]: https://blogs.oracle.com/bonwick/entry/128_bit_storage_are_you
