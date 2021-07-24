db.createUser(
    {
        user: 'biotablero',
        pwd: 'admin',
        roles: [ { role: "readWrite", db: "biotablero"} ] 
    }
)