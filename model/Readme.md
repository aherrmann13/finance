## Model

#### Summary

this folder contains the
[openapi](https://swagger.io/docs/specification/about/)
model spec to generate the api by both server and client. It uses
[swagger-cli](https://www.npmjs.com/package/swagger-cli)
to validate the schema and produce and output in a single
yaml file at ./dist/model.yaml

#### Commands

| run           | summary     |
| ------------- |-------------|
| `npm run validate` | validates the schema fits the openapi definition |
| `npm run generate` | generates the model.yaml file.  *this step does not validate*      |

#### Links
[swagger editor/viewer](https://editor.swagger.io/)
[github md editor/viewer](https://jbt.github.io/markdown-editor/)

#### Notes
<details>
  <summary>path generation is annoying</summary>
  <p>
    path generation is kindof messy and spread across
    multiple files, where openapi.yaml needs to reference
    the available parameters, and the definition in
    the file under ./paths/ must define the parameters.
    Becuase `$ref` does not allow merging of definitions,
    all operations with the same path and different http
    verbs must be under the same object, so we end up with
    `UpdateDeleteTransaction` as an object because they are
    `put` and `delete` under `transaction/{id}`
  </p> 
</details>
