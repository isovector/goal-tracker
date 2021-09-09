# goal-tracker

A little wrapper for manipulating conky goals.

What are conky goals? Little text files that I've hooked conky up to show ---
it's a nice way to keep quantifiable goals in front of you at all times.

The `conky-goals` script:

```bash
#!/usr/bin/bash

for FILE in /home/sandy/.goals/*; do


echo -n '${color grey}'

sed -n 1p $FILE | tr -d \\n
echo -n ": "

echo -n '${color}'

NUM=$(sed -n 3p $FILE | tr -d \\n)
DEN=$(sed -n 2p $FILE | tr -d \\n)

echo -n $NUM
echo -n "/"
echo -n $DEN

PERC=$(( (NUM * 100) / DEN ))

echo -n ' ${execbar echo '
echo -n $PERC
echo '}'

done
```

and you can hook it up to conky via:

```
${execpi 5 /home/sandy/.tino/bin/conky-goal}
```

