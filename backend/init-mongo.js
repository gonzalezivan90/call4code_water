db.createUser(
    {
        user: 'dbuser',
        pwd: 'admin',
        roles: [ { role: "readWrite", db: "dbuser"} ] 
    }
)