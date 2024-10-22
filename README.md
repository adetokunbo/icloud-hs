# icloud-auth - unofficial auth for iCloud services

[![GitHub CI](https://github.com/adetokunbo/icloud-auth/actions/workflows/ci.yml/badge.svg)](https://github.com/adetokunbo/icloud-auth/actions)
[![Stackage Nightly](http://stackage.org/package/icloud-auth/badge/nightly)](http://stackage.org/nightly/package/icloud-auth)
[![Hackage][hackage-badge]][hackage]
[![Hackage Dependencies][hackage-deps-badge]][hackage-deps]
[![BSD3](https://img.shields.io/badge/license-BSD3-green.svg?dummy)](https://github.com/adetokunbo/icloud-auth/blob/master/LICENSE)

`icloud-auth` allows logon to [iCloud] servers; upon provision of the username and
password, it retrieves and stores an authorization token, for use in programmatic
sessions with other iCloud services


## Warning - use at your own risk

- It is unofficial, *not* supported by Apple, and not guaranteed to work.

- The iCloud service APIs it uses are not officially documented and can be
changed at any time. Use at your own risk; in the worst case, Apple might ban
your account!


### Regular reauthentication 

Note: The retrieved authorization token expires, after which re-authentication
is required. There is no way to control to the duratiion, it is set by iCloud
itself. At the time of writing (2024/10). the duration is two months.


[hackage-deps-badge]: <https://img.shields.io/hackage-deps/v/icloud-auth.svg>
[hackage-deps]:       <http://packdeps.haskellers.com/feed?needle=icloud-auth>
[hackage-badge]:      <https://img.shields.io/hackage/v/icloud-auth.svg>
[hackage]:            <https://hackage.haskell.org/package/icloud-auth>
[iCloud]:             <https://www.icloud.com/>
