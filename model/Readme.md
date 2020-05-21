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
    path generation is kindof messy and spread across multiple files, where openapi.yaml needs to reference the
    available parameters, and the definition in the file under ./paths/ must define the parameters. Becauase
    <code>$ref</code> does not allow merging of definitions, all operations with the same path and different http verbs
    must be under the same object, so we end up with <code>UpdateDeleteTransaction</code> as an object because they are
    <code>put</code> and <code>delete</code> under <code>transaction/{id}</code>
  </p> 
</details>

<details>
  <summary>cannot use oneOf in swagger doc</summary>
  <h4>issue</h4>
  <p>
    using twilio guardrail to generate http4s models and endpoints. as of now, guardrail does not support
    <code>oneOf</code> in swagger model. <a href="https://github.com/twilio/guardrail/issues/195">this issue</a> here
    needs to be tracked to determine when <code>oneOf</code> can be used. the models that should be updated are
    <code>Record</code> to be oneOf <code>Asset</code>, <code>Transaction</code>, or <code>Transfer</code>
  </p>
  <h4><code>Record</code></h4>
  <p>
    setting <code>Record</code> to have three properties, asset, transaction, and transfer, with <code>Asset</code>,
    <code>Transaction</code>, and <code>Transfer</code> with the 'assumed' guarantee that it will only ever have one of
    those objects
  </p>
</details>

<details>
  <summary>must use allOf on single property to combine with readOnly or writeOnly</summary>
  <h4>issue</h4>
  <p>
    because <code>$ref</code> replaces all sibling properties with the contents of the ref it will replace the readOnly
    or writeOnly flags (or anything else that gets set).  The workaround is to add an <code>allOf</code> property with
    a single reference and add the required properties as siblings of the <code>allOf</code> prop.
    </br>
    sources <a href="https://stackoverflow.com/questions/51402156">here</a> and 
    <a href="https://github.com/OAI/OpenAPI-Specification/issues/1671">here</a>
  </p>
</details>
