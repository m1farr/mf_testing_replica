# mf_testing_replica: Overview

This repository contains a replication of the work that I did alongside the 
compliance team at Congress Asset Management. This project aimed to simplify
and automate the compliance testing process for mutual funds. 

Testing was previously run in Excel and relied upon data from APX and 
FactSet/Bloomberg. This repository also relies upon APX and Bloomberg files and
produces a similar results summary, but has streamlined the process of parsing 
files, making calculations, and performing tests for each fund.

*All data files are necessarily excluded from this repository.*

## Mutual fund tests

**Investment Limitations**

- Test 1: Funds must normally have at least 75% of its total assets invested in
Cash & Equities, Govt Issues, Funds, or Other Securities where the Fund holds
less than 10% of the issuers voting shares or invests less than 5% of total
assets.

- Test 1.d.i: Other securities limited in respect of any one issuer to an amount
not greater than 5% of its total assets.

- Test 1.d.ii: Other securities limited in respect of any one issuer to no 
greater than 10% of the outstanding voting securities of such issuer.


- Test 2.a: The fund cannot own more than 3% of the total outstanding voting stock
of another investment company.

- Test 2.b: The fund cannot invest more than 5% of its assets in securities of
another investment company.

- Test 2.c: The Fund cannot invest more than 10% of its assets in securities of
other investment companies in aggregate.

- Test 3: The Fund may not invest more than 10% of net assets in any one industry
(other than government securities.)

- Test 4: Fund family combined holdings of a closed end fund may not exceed 10% of
the total outstanding voting stock of the closed end investment fund.

- Test 5: Fund cannot own more than 10% of the outstanding voting stock of an 
insurance company.

- Test 6.a: The fund may acquire any security issued by a company that derives
15+% of gross revenues from securities related activities, provided that: the
acquiring company owns not more than 5% of the outstanding securities of that
class of the issuer's equity securities.

- Test 6.b: The fund may acquire any security issued by a company that derives
15+% of gross revenues from securities related activities, provided that: the
acquiring company has invested not more than 5% of the value of its total assets
in the securities of the issuer.

- Test 7: 80% of Fund's assets must be invested within the Market Cap Range of the
Relevant Benchmark.


**Non Fundamental Limitations**

- Test 1: List of issues where the fund owns >4.9% of outstanding shares, ranked
by percent.

- Test 2: The Fund may invest up to 15% of total assets in ADRs & GDRs.

- Test 3: The Fund may invest 10% of its total assets in ETFs with no single ETF
greater than 5% of its total assets.

- Test 4: The Fund may invest no more than 25% of its total assets in any one issuer.


**Restricted Assets**

Test 1: The fund may not:

- Purchase or sell Real Estate
- Purchase or sell Physical Commodities
- Make Loans of Money

Test 2: Jackson National Restricted Stocks (May Not Own List)
