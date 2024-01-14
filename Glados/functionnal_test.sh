#!/bin/bash
##
## EPITECH PROJECT, 2024
## Glados_mirror
## File description:
## functionnal_test
##

check_executable() {
    local current_directory="$(pwd)"
    local executable_name="$current_directory/glados"
    local executable="$1"

    # if command -v "$executable" >/dev/null 2>&1; then
    if command -v "glados" >/dev/null 2>&1; then
        echo -e "\033[32m✔\033[0m: L'exécutable glados est présent."
        return 0
    else
        echo -e "\033[31m✘\033[0m: Erreur : L'exécutable glados n'est pas trouvé."
        make re
        if command -v "glados" >/dev/null 2>&1; then
            echo -e "\033[32m✔\033[0m: L'exécutable glados a été créé avec succès."
            return 0
        else
            echo -e "\033[31m✘\033[0m: Erreur : Impossible de créer l'exécutable glados avec make."
            exit 1
        fi
    fi
}


run_test() {
    local file_path="$1"
    local expected_output="$2"
    local title="$3"
    local executable="./glados"

    actual_output="$($executable < "$file_path")"


    if [ "$actual_output" == "$expected_output" ]; then
        echo -e "\033[32m✔\033[0m: $title"
        printf "Expected output: $expected_output\n"
        printf "Actual output: $actual_output\n"
        return 0
    else
        echo -e "\033[31m✘\033[0m: $title"
        printf "Expected output: $expected_output\n"
        printf "Actual output: $actual_output\n"
        return 1
    fi
}

#check_executable "$executable_name"
run_test "test/test_simple.scm" "74" "Test simple add"
run_test "test/test_simple_mult.scm" "60" "Test simple mult"
run_test "test/test_simple_div.scm" "5" "Test simple div"