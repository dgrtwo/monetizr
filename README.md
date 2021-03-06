<!-- README.md is generated from README.Rmd. Please edit that file -->



## The monetizr package: bringing advertising to R

The monetizr package lets you monetize an R package through advertisements.

### Installation

Install the package from GitHub with [devtools](github.com/hadley/devtools):


```r
devtools::install_github("dgrtwo/monetizr")
```

### Advertise functions in a package

Suppose you've created a function:


```r
multiply_by_two <- function(x) {
  x * 2
}
```

This function takes its input and multiplies it by two. By enterprise software standards this is quite useful.

You've shared your function and people are using it: 


```r
multiply_by_two(10)
#> [1] 20
```

But every time they use it, you are not capturing the value- you are in fact getting nothing from it. What a ripoff!

### Marketably

The monetize package provides the `marketably` function, which modifies a function to display an advertisement. (Why "marketably"? Because functions that return other functions should be adverbs). You can provide ads for paying third parties, or you can advertise your own work. Here I'll add an advertisement for my blog:


```r
ad <- "Check out my blog, Variance Explained, at www.varianceexplained.org!"
multiply_by_two_enterprise <- marketably(multiply_by_two, ad)
```

Now, when anyone calls it:


```r
multiply_by_two_enterprise(10)
#> Check out my blog, Variance Explained, at www.varianceexplained.org!
#> [1] 20
```

You've captured some value!

Also try the `html = TRUE` argument. Now thanks to htmltools, this will pop up in the user's browser or RStudio "Viewer" window, where it is even harder for your customers to ignore.


```r
ad <- paste("Check out my blog, ",
            "<a href='www.varianceexplained.org!'>Variance Explained</a>")
multiply_by_two_enterprise <- marketably(multiply_by_two, ad, html = TRUE)

multiply_by_two_enterprise(20)
#> [1] 40
```

You can also insert a delay (in seconds) after the ad is shown with `pause`, much like YouTube's ads:


```r
ad <- "Check out my blog, Variance Explained!"
multiply_by_two_enterprise <- marketably(multiply_by_two, ad, pause = 3)

multiply_by_two_enterprise(20)
#> Check out my blog, Variance Explained!
#> [1] 40
```

### Monetize an entire package

Usually you'll want to monetize your entire package, not just a single function. So monetizr provides a way to make this convenient- `market_all`:


```r
market_all(ad = "Have I mentioned my blog, Variance Explained?")
```

Place this in the zzz.R file of your package to add advertising to all your functions. ([See here for more on zzz.R](http://r-pkgs.had.co.nz/r.html)).

By next week all of my packages on GitHub and CRAN will be monetized in this way, and I'm counting on others in the community to follow. 

### Impression Analytics

Ever since I entered industry I appreciate the importance of analytics: understanding how many impressions you are receiving, not just the number of clicks. Here those impressions are represented by the number of times each of your monetized functions is used.

The monetizr package supports this core business need. Simply define a route on your own website or back-end server to record an impression, and then your function will fire off a request to that site on each call:


```r
url <- "http://example.com/impressions/record"
ad <- "Check out my blog, Variance Explained, at www.varianceexplained.org!"
multiply_by_two_enterprise <- marketably(multiply_by_two, ad,
                                         impression_url = url)
```

This will send a GET request (with the response discarded) to 

    http://example.com/impressions/record?function=multiply_by_two&id=1

(Where `id` is the index of the ad that was shown, among the choices given as `ad`).

### Upcoming Features

Functions that are currently in development for monetizr include:

* `expensively`: Have a function require a bitcoin payment with each call
* `temporarily`: Have a function offer a "free trial" for a number of uses or time period before requiring payment
* `premiumly`: Have a function require payment to activate particular options.

If you have other suggestions, please open a GitHub Issue, or better yet hop on a call with one of our sales staff!
