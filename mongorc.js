var access = db.runCommand({connectionStatus : 1});

var whoami = db.runCommand({connectionStatus : 1}).authInfo.authenticatedUsers[0]["user"];

// Gives current user, read priveleges if they are an admin for blue-review db
// use blue-review; readme();
var readme = function() {
  db.grantRolesToUser(whoami, ["readWrite", { role: "read", db: "blue-review" } ], {});
}
