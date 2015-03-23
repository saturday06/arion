# Arion 

<img src="http://s1.hubimg.com/u/7984120_f260.jpg"
 alt="Arion Image" title="arion" align="right" height="320px"/>

> Homer said in Iliad

> ...there is no man that shall catch thee by a burst of speed, neither pass thee by,
> nay, not though in pursuit he were driving goodly Arion,
> the swift horse of Adrastus, that was of heavenly stock...
> Homer


**Arion is a helps you make red-green-refactor quick and efficient when working with hspec tests**

**It watches the file systems for change in source or test code and selectively runs hspec tests**


# Note
This is a work in progress. The runner is just a prototype. As of now Arion needs two threads. 

If you wish to try it out then clone the repo and follow the instructions below:

```cabal
cabal update
cabal install
arion <folder-to-watch> +RTS -N2
```
