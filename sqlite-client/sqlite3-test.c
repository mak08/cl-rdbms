#include <stdio.h>
#include <sqlite3.h>

int main(int argc, char **argv){

    sqlite3 *db;
    int rc;

    char *db_name= "sqlite3-test.sdb";

    rc = sqlite3_open(db_name, &db);

    if (rc != SQLITE_OK) {
        fprintf(stderr, "failed to open in memory database: %s\n", 
                sqlite3_errmsg(db));
        sqlite3_close(db);
        return(1);
    }

    const char *create_sql = "CREATE TABLE foo(bar TEXT)";
    sqlite3_stmt *statement;

    rc = sqlite3_prepare_v2(db, create_sql, -1, &statement, NULL);

    if (rc != SQLITE_OK) {
        fprintf(stderr, "failed to prepare statement: %s\n",
                sqlite3_errmsg(db));
        sqlite3_close(db);
        return(1);
    }

    rc = sqlite3_step(statement);

    if (rc == SQLITE_ERROR) {
        fprintf(stderr, 
                "failed to execute statement: %s\n", 
                sqlite3_errmsg(db));
    }

    sqlite3_close(db);

}
