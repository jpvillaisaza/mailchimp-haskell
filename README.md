# Haskell bindings for the MailChimp API

[![][1]][0]

[0]: https://circleci.com/gh/jpvillaisaza/mailchimp-haskell
[1]: https://circleci.com/gh/jpvillaisaza/mailchimp-haskell.svg?style=svg

![][2]
![][3]

[2]: https://www.stackage.org/package/mailchimp/badge/lts
[3]: https://www.stackage.org/package/mailchimp/badge/nightly

## Example

To get started, create a manager using a library or the provided
function to do so.

Right now, this supports basic access authentication, so you need an
API key.

You need a MailChimp API key. Go to MailChimp and then to your
account. This is the minimum needed to get started.

In order to see an interesting example, you also need a list ID.

```
  let
    MainClient {..} = makeMainClient
```

A main client includes the required function to create an auth client
which takes your key.

```
  let
    AuthClient {..} = makeAuthClient (BasicAuthData "" key)
```

Alternatively, you can simply use the following and avoid the two
steps above:

```
  let
    AuthClient {..} = makeAuthClientWithKey key
```

This gives you access to the whole API, but we're only interested in
the list member subresource, so let's get more specific:

```
    ListMemberClient {..} = makeListMemberClient listId
```

And now we can start using the functions to make calls to MailChimp.

To run bindings, we can use run. For example, let's get all list
members for our list. Note that the list Id is not here, it's already
there for all calls. The manager, though, is not here, and the key is
duplicated, which should be fixed:

```
  sd <- run manager key getListMembers
```

## Coverage

- API Root
- Lists
  - Members

## License

Licensed under the MIT license. See [LICENSE.md](LICENSE.md).

[mailchimp]: http://mailchimp.com/
[mailchimp-api]: http://developer.mailchimp.com/
