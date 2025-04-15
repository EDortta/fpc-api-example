CREATE USER IF NOT EXISTS 'dev-empresas'@'%' IDENTIFIED WITH mysql_native_password BY 'devpass123';
GRANT ALL PRIVILEGES ON empresas_db.* TO 'dev-empresas'@'%';
FLUSH PRIVILEGES;
