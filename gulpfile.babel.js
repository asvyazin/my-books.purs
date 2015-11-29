import gulp from "gulp";
import purescript from "gulp-purescript";
import webpack from "webpack-stream";


let mySources = [
    "src/**/*.purs"
];


let depsSources = [
    "bower_components/purescript-*/src/**/*.purs"
];


let sources = mySources.concat(depsSources);


let jsSources = [
    "src/**/*.js",
    "bower_components/purescript-*/src/**/*.js"
];


gulp.task("make", function() {
  return purescript.psc({
    src: sources,
    ffi: jsSources
  });
});


let mkBundleTask = function (name, main) {

  gulp.task("prebundle-" + name, ["make"], function() {
    return purescript.pscBundle({
      src: "output/**/*.js",
      output: "tmp/js/" + name + ".js",
      module: main,
      main: main
    });
  });

  gulp.task("bundle-" + name, ["prebundle-" + name], function () {
    return gulp.src("tmp/js/" + name + ".js")
      .pipe(webpack({
        resolve: { modulesDirectories: ["node_modules"] },
        output: { filename: name + ".js" }
      }))
      .pipe(gulp.dest("public/js"));
  });

  return "bundle-" + name;
};


let mkBundleServerTask = function (name, main) {

    gulp.task("bundle-" + name, ["make"], function () {
	let srcFilename = "output/" + main + "/" + name + ".js";
	return gulp.src(srcFilename)
	    .pipe(webpack({
		resolve : { modulesDirectories: ["output"] },
		output : { filename: name + "-server.js" }
	    }))
	    .pipe(gulp.dest("public/js"));
    });

    return "bundle-" + name;
};


gulp.task("bundle", [
    mkBundleTask("index", "Entries.Index"),
    mkBundleServerTask("index", "Entries.Index.Server"),
    mkBundleTask("login", "Entries.Login")
]);


let mkWatch = function(name, target, files) {
    gulp.task(name, [target], function() {
	return gulp.watch(files, [target]);
    });

    return name;
};


let allSources = sources.concat(jsSources);


gulp.task("watch", [
    mkWatch("watch-index", "bundle-index", allSources),
    mkWatch("watch-index-server", "bundle-index-server", allSources),
    mkWatch("watch-login", "bundle-login", allSources)
]);


gulp.task("default", ["bundle"]);
