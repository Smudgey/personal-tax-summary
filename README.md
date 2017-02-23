# personal-tax-summary

[![Build Status](https://travis-ci.org/hmrc/personal-tax-summary.svg)](https://travis-ci.org/hmrc/personal-tax-summary) [ ![Download](https://api.bintray.com/packages/hmrc/releases/personal-tax-summary/images/download.svg) ](https://bintray.com/hmrc/releases/personal-tax-summary/_latestVersion)

Person Tax Summary is a microservice that facilitates sharing of the TAI domain model

Requirements
------------

The following services are exposed from the micro-service.

API
---

| *Task* | *Supported Methods* | *Description* |
|--------|----|----|
| ```/personal-tax/:nino/buildestimatedincome  ``` | POST | Given a POST body of ```TaxSummaryDetails``` [More...](docs/tax-summary-details-example.md)  this returns the ```EstimatedIncomeViewModel``` |
| ```/personal-tax/:nino/buildyourtaxableincome``` | POST | Given a POST body of ```TaxSummaryDetails``` [More...](docs/tax-summary-details-example.md)  this returns the ```YourTaxableIncomeViewModel```|

# Sandbox
All the above endpoints are accessible on sandbox with `/sandbox` prefix on each endpoint,e.g.
```
    POST /sandbox/personal-tax/:nino/buildestimatedincome    
```

### License

This code is open source software licensed under the [Apache 2.0 License]("http://www.apache.org/licenses/LICENSE-2.0.html")
