# Lessons learned

- test for obvious edge cases first
- searching might be sped up with
    - sorting the array
    - creating a hash-map
- avoid running more than you need
    - e.g. instead of `list.filter(cond).nonEmpty` use `list.exist(cond)`    