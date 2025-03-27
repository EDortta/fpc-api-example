This project was created to help some Pascal programmers to develop microservices using Codium as the IDE

1. Build this project
    ```bash
    docker compose build
    ```

2. Run this project
    ```bash
    docker compose up
    ```

3. Access the VSCodium IDE using the following URL:
    ```bash
    http://localhost:8123
    ```

4. Update the modules and libraries into the container using the integrated terminal
    ```bash
    cd ~/scripts
    bash update.sh
    ```
    **This step is very important to keep the project updated.** So each time you update the project, you must run this script.

5. Test the compilation of each example. For example: **PingServer.pas**
    ```bash
    cd ~/Projects/api-test
    fpc PingServer
    ```

6. Run the example:
    ```bash
    ./PingServer
    ```

7. Test the API using the following URL:
    ```bash
    http://localhost:8321/ping
    ```

8. Stop the example using `ctrl-c` in the integrated terminal.

9. Repeat the steps 5 to 8 for each example: AuthServer and JWTServerTest.
    