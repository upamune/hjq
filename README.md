# hjq

`jq` もどき

```console
$ echo '[ { "age": 27, "name": "ファーストサマーウイカ", "tel-number": "111-1111" }, { "age": 25, "name": "ヒラノノゾミ", "tel-number": "222-2222" }, { "age": 22, "name": "芹澤優", "tel-number": "333-3333" } ]' \
> | hjq '{"name":.[2].name,"tel-numer": .[2].tel-number}'

{
    "tel-numer": "333-3333",
    "name": "芹澤優"
}
```