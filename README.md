This project was created to help some Pascal programmers to develop microservices using Codium as the IDE

## How to use
1. Clone this project
    ```bash
    cd ~
    git clone https://github.com/EDortta/fpc-api-example.git
    cd fpc-api-example
    ```

2. Build this project
    ```bash
    docker compose build
    ```

3. Run this project
    ```bash
    docker compose up
    ```

4. Access the VSCodium IDE using the following URL:
    ```bash
    http://localhost:8123
    ```

5. Update the modules and libraries into the container using the integrated terminal
    ```bash
    cd ~/scripts
    bash update.sh
    ```
    **This step is very important to keep the project updated.** So each time you update the project, you must run this script.

6. Test the compilation of each example. For example: **PingServer.pas**
    ```bash
    cd ~/Projects/api-test
    fpc PingServer
    ```

7. Run the example:
    ```bash
    ./PingServer
    ```

8. Test the API using the following URL:
    ```bash
    http://localhost:8321/ping
    ```

9. Stop the example using `ctrl-c` in the integrated terminal.

10. Repeat the steps 5 to 8 for each example: AuthServer and JWTServerTest.
    