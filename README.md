# exphist
Implementation of exponential histograms as proposed by Datar et al (2001).

### What is an exponential histogram?
The exponential histogram algorithm is presented in [Maintaining Stream Statistics over Sliding Windows](http://www-cs-students.stanford.edu/~datar/papers/sicomp_streams.pdf) by Datar, Gionis, Indyk and Motwani.

An exponential histogram is a sliding window counter that can be used to compute statistics over data streams guaranteeing a bounded relative error. At initialization time, it is possible to configure the data structure with

- **winsize:** size of the sliding window
- **epsilon:** maximum relative error
- **duration:** time span for time-based buckets

Possible interaction with the data structure are: inserting a number into the exponential histogram, querying it for an approximate counts with guess or for the current size of the sliding window.

### How to use an exponential histogram?
Here's a code snippet demonstrating how to use the exponential histogram: 

```R
# initialization with standard parameters (winsize=100, epsilon=0.05)
eh <- exphist()

# inserting a value into the exponential histogram
eh <- insert(eh, 1)
size(eh)
value(eh)

# zeroes are discarded
eh <- insert(eh, 0)
size(eh)
value(eh)

# inserting another value into the exponential histogram
eh <- insert(eh, 1)
size(eh)
value(eh)
```
