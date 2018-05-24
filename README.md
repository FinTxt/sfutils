# FinTxtUtils

The goal of FinTxtUtils is to provide code and documentation on working with [semantic fingerprints](http://www.cortical.io/technology_semantic.html) in R. The package contains functions that communicate with the Cortical free API, classes and functions that allow a user to work with fingerprints in R, a shiny app to visualize fingerprints and a host of distance and similarity metrics that can be used to classify and compare fingerprints. 

## Installation

You can install the latest stable version of FinTxtUtils from the [github repository](https://github.com/JasperHG90/FinTxtUtils) with:

```r
devtools::install_github("JasperHG90/FinTxtUtils()")
```

You might need to authenticate with bitbucket (see `?devtools::install_github()`)

## Dependencies

All R dependencies are installed when the package is installed.

FinTxtUtils uses `reticulate` to create a python environment and call the cortical API. You need to install the `retinasdk` module using 

```terminal
pip install retinasdk
```

## Docker image

You can run the package in a docker container. Issue:

```terminal
docker pull jhginn/fintxtdocker
```

## Examples

Here are some basic examples for working with FinTxtUtils. (you can find more elaborate examples under docs/examples).

After loading the library, you can use the 'company_descriptions' dataset (incorporated in the package) to play around with the functionality of the package.

```r
rm(list=ls())
library(FinTxtUtils)

# Load data
data("company_descriptions")
```

We can fingerprint each of these descriptions using the following command:

```r
fps <- lapply(company_descriptions, function(x) Document(x$desc))
fps[[1]]
```

```text
Document object.

 unique id: 	2dbbb3f7-85b4-40b0-b794-3215d55354b3
 type: 		    document
 fingerprint: 984 positions
```

Each individual fingerprint can be plotted:

```r
plot(fps[[1]])
```

You can also create a 'Collection' of documents. A collection is a group of individual fingerprints that can be plotted to inspect the overlap between the fingerprints:

```r
fps_col <- Collection(fps)
plot(fps_col)
```

In this case, it could be informative to split the descriptions into e.g. 'finance', 'information technology' and 'consumer goods' and to create a collection for each.

Collections can be converted to sparse binary matrices by using the canonical `as.matrix()` command:

```r
fps_sparse <- as.matrix(fps_col)
print(fps_sparse)
```

```text
* Sparse matrix (limited to a matrix 10x10) = 
                                     p1 p2 p3 p4 p5 p6 p7 p8 p9 p10
2dbbb3f7-85b4-40b0-b794-3215d55354b3 0  0  0  0  0  0  0  1  1  0  
a229a111-5e65-4129-98b8-c7db9edc649b 0  0  0  0  0  0  0  1  0  0  
ba732a75-5c1f-4a77-b33a-76fe6cf504b3 0  0  0  0  0  0  1  1  1  0  
f6584478-2d19-4862-9c64-f1be6974b468 0  0  0  1  0  0  0  0  1  0  
f74aba09-af33-45f1-887a-c03dbb1ad943 1  1  1  1  0  1  1  1  1  0  
db1d76a2-fd02-48bc-bdb3-98b20fbea24f 0  0  0  1  0  0  0  1  1  0 
```

We can now compare these fingerprints to one another using the built-in similarity and distance metrics:

```r
# Create an empty matrix
m <- matrix(0, ncol = 6, nrow = 6)
# Populate
for(i in 1:6) {
  m[,i] <- compare(fps_sparse, fps[[i]], metric = "cosine")[,1]
}
colnames(m) <- names(company_descriptions)
rownames(m) <- names(company_descriptions)
print(m)
```

```text
          unilever     ahold       ing  Alphabet     apple  jpmorgan
unilever 1.0000000 0.2784553 0.3475610 0.3140244 0.3109756 0.3140244
ahold    0.2784553 1.0000000 0.3048780 0.2540650 0.2256098 0.2571138
ing      0.3475610 0.3048780 1.0000000 0.3810976 0.3313008 0.5853659
Alphabet 0.3140244 0.2540650 0.3810976 1.0000000 0.5264228 0.3882114
apple    0.3109756 0.2256098 0.3313008 0.5264228 1.0000000 0.3414634
jpmorgan 0.3140244 0.2571138 0.5853659 0.3882114 0.3414634 1.0000000
```

We can also choose another metric to compare the fingerprints

```r
# Create an empty matrix
m <- matrix(0, ncol = 6, nrow = 6)
# Populate
for(i in 1:6) {
  m[,i] <- compare(fps_sparse, fps[[i]], metric = "dennis")[,1]
}
colnames(m) <- names(company_descriptions)
rownames(m) <- names(company_descriptions)
print(m)
```

```text
          unilever     ahold       ing  Alphabet     apple  jpmorgan
unilever 120.31250  27.95478  36.80030  32.50762  32.11738  32.50762
ahold     27.95478 120.31250  31.33689  24.83283  21.19055  25.22307
ing       36.80030  31.33689 120.31250  41.09299  34.71900  67.23933
Alphabet  32.50762  24.83283  41.09299 120.31250  59.69461  42.00356
apple     32.11738  21.19055  34.71900  59.69461 120.31250  36.01982
jpmorgan  32.50762  25.22307  67.23933  42.00356  36.01982 120.31250
```

See `?FinTxtUtils::compare()` to view a list of supported similarity and distance metrics.

Instead of comparing the documents to each other, we can define a filter. This will return a fingerprint for that specific filter which we can then use to compare to other documents:

```r
# Based on snippets taken from wikipedia (https://en.wikipedia.org/wiki/Information_technology)
filt <- Filter(name = "tech",
               positive = c("Information technology (IT) is the use of computers to store, retrieve, transmit, and manipulate data,[1] or information, often in the context of a business or other enterprise.[2] IT is considered to be a subset of information and communications technology (ICT).",
                            "The term is commonly used as a synonym for computers and computer networks, but it also encompasses other information distribution technologies such as television and telephones. Several products or services within an economy are associated with information technology, including computer hardware, software, electronics, semiconductors, internet, telecom equipment, and e-commerce.[5][a]",
                            "Database management systems emerged in the 1960s to address the problem of storing and retrieving large amounts of data accurately and quickly. One of the earliest such systems was IBM's Information Management System (IMS),[23] which is still widely deployed more than 50 years later.[24] IMS stores data hierarchically,[23] but in the 1970s Ted Codd proposed an alternative relational storage model based on set theory and predicate logic and the familiar concepts of tables, rows and columns. The first commercially available relational database management system (RDBMS) was available from Oracle in 1980.",
                            "The relational database model introduced a programming-language independent Structured Query Language (SQL), based on relational algebra. The terms 'data' and 'information' are not synonymous. Anything stored is data, but it only becomes information when it is organized and presented meaningfully.[28]:1–9 Most of the world's digital data is unstructured, and stored in a variety of different physical formats[29][b] even within a single organization. Data warehouses began to be developed in the 1980s to integrate these disparate stores. They typically contain data extracted from various sources, including external sources such as the Internet, organized in such a way as to facilitate decision support systems (DSS)",
                            "Data transmission has three aspects: transmission, propagation, and reception.[31] It can be broadly categorized as broadcasting, in which information is transmitted unidirectionally downstream, or telecommunications, with bidirectional upstream and downstream channels.[21]",
                            "Hilbert and Lopez identify the exponential pace of technological change (a kind of Moore's law): machines' application-specific capacity to compute information per capita roughly doubled every 14 months between 1986 and 2007; the per capita capacity of the world's general-purpose computers doubled every 18 months during the same two decades; the global telecommunication capacity per capita doubled every 34 months; the world's storage capacity per capita required roughly 40 months to double (every 3 years); and per capita broadcast information has doubled every 12.3 years."),
               negative = c("music"))
# print
print(filt)
```

```text
Filter object.

 unique id: 	9652a3ae-71f5-4983-bf38-69486cbedbd6
 type: 		filter
 fingerprint: 	984 positions
```

We compare the articles to the filter:

```r
cmp <- compare(fps_sparse, filt)
rownames(cmp) <- names(fps)
colnames(cmp) <- "tech filter"
print(cmp)
```

```text
         tech filter
unilever   0.2896341
ahold      0.2154472
ing        0.3597561
Alphabet   0.4542683
apple      0.5467480
jpmorgan   0.3750000
```

If we wanted to classify the documents, we could create three filters and compare the documents to each:

```r
# Taken from wikipedia (https://en.wikipedia.org/wiki/Finance)
filt_finance <- Filter("finance",
                       "positive" = c("Finance is a field that deals with the study of investments. It includes the dynamics of assets and liabilities over time under conditions of different degrees of uncertainties and risks. Finance can also be defined as the science of money management. Market participants aim to price assets based on their risk level, fundamental value, and their expected rate of return. Finance can be broken into three sub-categories: public finance, corporate finance and personal finance."),
                       "negative" = c("Music"))

# Snippets from wikipedia (https://en.wikipedia.org/wiki/Fast-moving_consumer_goods)
filt_consumergoods <- Filter("consumer goods",
                             "positive" = c("Fast-moving consumer goods (FMCG) or consumer packaged goods (CPG) are products that are sold quickly and at relatively low cost. Examples include non-durable goods such as packaged foods, beverages, toiletries, over-the-counter drugs and many other consumables.[1][2] In contrast, durable goods or major appliances such as kitchen appliances are generally replaced over a period of several years.

Many fast moving consumer goods have a short shelf life, either as a result of high consumer demand or because the product deteriorates rapidly. Some FMCGs, such as meat, fruits and vegetables, dairy products, and baked goods, are highly perishable. Other goods, such as pre-packaged foods, soft drinks, chocolate, candies, toiletries, and cleaning products, have high turnover rates. The sales are sometimes influenced by holidays and seasons."),
"negative" = c("Music"))
```

Compare each to the filter:

```r
cmp_all <- do.call(cbind.data.frame, list(
  compare(fps_sparse, filt),
  compare(fps_sparse, filt_finance),
  compare(fps_sparse, filt_consumergoods)
))
colnames(cmp_all) <- c("tech", "finance", "consumer_goods")
rownames(cmp_all) <- names(fps)
print(cmp_all)
```

```text
              tech   finance consumer_goods
unilever 0.2896341 0.3014580      0.3795488
ahold    0.2154472 0.1992335      0.2260701
ing      0.3597561 0.4620966      0.3588084
Alphabet 0.4542683 0.3171046      0.3691786
apple    0.5467480 0.2732941      0.3328830
jpmorgan 0.3750000 0.4860880      0.3691786
```

As we can see, the first two filters are pretty good, but the consumer goods one is unclear.

## Performance

In the figure below, performance is shown for two functions (`as.matrix()` and `compare()`) and the object size of the sparse binary matrix created by `as.matrix()`. These were tested on a 2014 Macbook pro with a dual-core 2.6 GHz i5 processor. 

<img src=plots/speed.png width=80%/>

`as.matrix()` takes roughly 10 minutes for 100.000 documents and takes up 1.1GB of space
comparing a document to 100.000 documents takes roughly 6 minutes. By comparison, this operation takes +- 25 seconds on a collection of 10.000 documents (size is +-123MB). These are not great performance scores if you want to analyses on large sets of documents. Given the high dimensionality, however, it is quite efficient.

## See also

* Documentation of [Cortical free API](http://documentation.cortical.io/intro.html#what-is-a-semantic-fingerprint)
* Online [tool](http://api.cortical.io) to test the Cortical API
