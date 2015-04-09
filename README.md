A rebar3 plugin to enable a nice tidy deps specification for common case
Erlang projects.

## Description

The standard rebar syntax for dependency specification is noisy and in
many projects the list of dependencies often involves the exact same
pattern repeated numerous times. This plugin aims to remove some of
the clutter from the deps specification through a more clear and
concise syntax given that the following is true about the dependency
specification:

1. It points to a project that resides on github (public or private repo)
1. It uses `".*"` as the version regex
1. It does not employ any other dependency options such as `raw`

For a dependency that meets these criteria this plugin make it
possible specify dependencies using the following alternative syntax:

```
{mydep, {github, "kellymclauglin/mydep.git", {tag, "1.0.1"}}}
```

For private repos the syntax is similar:

```
{myprivatedep, {github_private, "kellymclauglin/mydep.git", {tag, "2.0.2"}}}
```

A version regex of `".*"` is used and the repo name is appended to the
`git://github.com/` URL or `git@github.com:` in the case of a private
repo.

The syntax can be used for all of the dependencies for a project or it
can be used for only selected dependencies. There is no issue with
inter-mingling with deps using the standard specification syntax. It
also works with profile dependencies.

## Configuration

Configure the plugin and and set it as a pre hook for the
`app_discovery` and `install_deps` providers by adding the following
to the rebar.config file:

```
{plugins, [
    {rebar_tidy_deps, ".*", {git, "git://github.com/kellymclaughlin/rebar3-tidy-deps-plugin.git", {tag, "0.0.2"}}}
]}.
```

## Example

If a project's deps specification was the following:

```
{deps, [
        {lager, ".*", {git, "git://github.com/basho/lager.git", {tag, "2.1.1"}}},
        {hackney, ".*", {git, "git://github.com/benoitc/hackney.git", {tag, "1.0.6"}}},
        {jsx, ".*", {git, "git://github.com/talentdeficit/jsx", {tag, "v2.5.2"}}}
       ]}.
```

The converted syntax would be:

```
{deps, [
        {lager, {github, "basho/lager.git", {tag, "2.1.1"}}},
        {hackney, {github, "benoitc/hackney.git", {tag, "1.0.6"}}},
        {jsx, {github, talentdeficit/jsx", {tag, "v2.5.2"}}}
       ]}.
```

If you are using a test profile to pull in test-only depencencies then
the same conversion applies.

This test profile dep specification:

```
{profiles, [
            {test, [
                    {deps, [
                            {meck, ".*", {git, "git://github.com/eproxus/meck.git", {tag, "0.8.2"}}}
                           ]}
                   ]}
           ]}.
```

becomes this:

```
{profiles, [
            {test, [
                    {deps, [
                            {meck, {github, "eproxus/meck.git", {tag, "0.8.2"}}}
                           ]}
                   ]}
           ]}.
```
