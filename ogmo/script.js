function hook() {
  const fs = require('fs');
  const path = require('path');

  return {
      beforeSaveProject: (project, data) => {
          const scriptsPath = path.dirname(project.path) + "/../scripts/";
          const entityPath = scriptsPath + "entities/";
          const makeData = modulename => "{-# LANGUAGE NoImplicitPrelude #-}\nmodule " + modulename + " where\n\nimport ScriptPrelude\n\n\nentityScript :: EntityScript\nentityScript = defaultScript"
          const andThen = f => err => {
              if (err === null || err.code == "EEXIST") {
                  f();
              } else {
                  console.log(err)
              }
          };
          fs.mkdir(scriptsPath, andThen(() =>
              fs.mkdir(
                  entityPath,
                  andThen(() => {
                  
                  let entities = data.entities
                  let entityList = entities.map(e => {return {modulename: e.name + e.exportID, id: e.exportID} });

                  entityList.forEach(e => {
                      let filename = entityPath + e.modulename + ".hs";
                      fs.writeFile(filename, makeData(e.modulename), {
                          flag: "wx"
                      }, andThen(x => {
                          return;
                      }));
                  })

                  const imports =  entityList.reduce((acc, val) => acc + "import qualified " + val.modulename + "\n", "");
                  const moduleString = entityList.reduce ((acc, val) => acc + "(\""+ val.id +"\"," + val.modulename + ".entityScript):", "")

                  const scriptData = "module Scripts where\n\nimport qualified Data.HashMap.Strict as HM\nimport qualified Data.Text as T\nimport ScriptPrelude\n\n" + imports + "\nscriptMap :: HM.HashMap T.Text EntityScript\n" + "scriptMap = HM.fromList $ " + moduleString + "[]"
                  fs.writeFile(entityPath + "Scripts.hs", scriptData, {
                        flag: "w"
                  }, x => {x != null ? console.log(x) : null; });
            }
                  )
              )));

          return data;
      }
  }
}

hook();