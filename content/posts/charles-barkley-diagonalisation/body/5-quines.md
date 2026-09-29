**Quines** are computer programs that ignore all inputs, and output their source code in a curious act of self-replication. For example, this python program:

```python
c = 'c = %r; print(c %% c)'; print(c % c)
```

(Verify it by executing it yourself in a python REPL or [this online editor](https://www.online-python.com/))
