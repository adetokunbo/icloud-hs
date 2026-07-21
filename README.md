# icloud-hs — unofficial Haskell client for iCloud services

`icloud-hs` is a collection of Haskell libraries and command-line tools for
accessing iCloud services using an Apple ID.


## Warning — use at your own risk

- These libraries are **unofficial** and not supported by Apple.
- The iCloud APIs they use are undocumented and may change without notice.


## Packages

| Package | Description |
|---------|-------------|
| [`icloud-auth`](icloud-auth/#readme) | Authenticate with iCloud using Apple ID credentials |
| [`icloud-drive`](icloud-drive/#readme) | Browse and download files from iCloud Drive |
| [`icloud-notes`](icloud-notes/#readme) | Read notes and folders from iCloud Notes |

`icloud-drive` and `icloud-notes` both depend on `icloud-auth` for
authentication.  Start there.


## Credits

The iCloud API behaviour documented and implemented here was derived from
studying these projects:

- [pyicloud](https://github.com/timlaing/pyicloud) — Python iCloud client
- [icloudpy](https://github.com/mandarons/icloudpy) — Python iCloud client (icloudpy fork)
- [fastlane](https://github.com/fastlane/fastlane) — iOS/macOS automation tools, whose
  Spaceship library implements iCloud authentication


## License

BSD-3-Clause
