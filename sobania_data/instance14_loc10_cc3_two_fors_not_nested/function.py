# Metrics:
# - Score: 3.00/3.00
# - Lines: 10
# - Complexity: 3
# - Structure: two_fors_not_nested
# - LineRelevance: 1.00
# - InputRelevance: 1.00
# - LineCoverage: 1.00
# - OutputVariability: 0.67
# - Seed: 176

def generated_func(arg0: int, arg1: int, arg2: int) -> int:
    var0 = 1
    for i0 in range(abs(arg2) + 1):
        var0 //= (i0 + 1) % 4 + 2
    var1 = 2
    for i1 in range(abs(var0) + 1):
        var1 = var1 - i1
        var1 //= 5 - abs(arg1) % 3
        var1 -= 2
    var1 -= arg0
    return var1