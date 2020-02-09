run

./lambda -o ./tests/workbench ./tests/workbench.lambda; ./tests/workbench

from the root directory, you should get

/tmp/tmp.BYdVjuWNfr/lambda.S: Assembler messages:
/tmp/tmp.BYdVjuWNfr/lambda.S:109: Error: no such instruction: `tstq %rdx,%rdx'
bash: ./tests/workbench: No such file or directory

