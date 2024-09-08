def permut_in(p: tuple[int, int, int], s: set[tuple[int, int, int]]):
    return (p in s or 
           (p[0], p[2], p[1]) in s or 
           (p[1], p[0], p[2]) in s or 
           (p[1], p[2], p[0]) in s or
           (p[2], p[0], p[1]) in s or
           (p[2], p[1], p[0]) in s) 
    

def count_total_cubes(n: int) -> int:
    def _gen():
        for i in range(1, n + 1):
            for j in range(1, n + 1):
                for k in range(1, n + 1):
                    yield (i, j, k)
    
    def _count() -> int:
        counted: set[tuple[int, int, int]] = set()

        count = 0
        
        for triplet in _gen():
            if not permut_in(triplet, counted):
                # print(triplet)
                count += 1
                counted.add(triplet)
        return count

    return _count()
    

print(count_total_cubes(10))
print((10 * 10 * 10))
