# Circuits Up!

[![Build Status](https://travis-ci.com/theIntelligentBook/circuitsup.svg?branch=master)](https://travis-ci.com/theIntelligentBook/circuitsup)

This is a set of videos and interactive exercises that try to teach the basics of computer architecture, beginning from circuits upwards. The intention is to get about as far as how a 1980s computer works. 

The site is built in Scala.js, and auto-deployed via Travis CI to GitHub Pages here: [Circuits Up!](https://theintelligentbook.com/circuitsup/)

It's also part of the Intelligent Book project. This is a re-imagining of some work on explorable explanations and smart materials that understand what they teach from my PhD. My PhD was earlier in the 2000s, when the web was a very different place. What would intelligent materials look like now that the client side can do so much more?

It uses a bespoke Scala front end toolkit, [Veautiful](https://github.com/wbillingsley/veautiful) that takes inspiration from various front end technologies: the original components from the Intelligent Book, but also modern front end frameworks such as React and Vue. The circuit toolkit, Wren, is copied from my Veautiful project and the changes will make their way back there again soon.

Most of the exercises in Circuits Up use a constraint propagator. This is not a full circuit simulator - it can't solve every circuit, but it does track *why* particular variables should have particular values. The first part of the original Intelligent Book project looked at animating these sorts of explanations across diagrams, to relate AI thinking to human thinking. 

The "challenge" layout, which uses CSS scaling to make the layout believe it is 1920 x 1080, is based on a combination of factors. First, that most "responsive" learn-to-code environments seem to break down if the aspect ratio changes *too much*. Second, because it lets the challenges mimic lecture slides and I thought the "bringing slides to life" aspect of it might be interesting.
