# Metrics:
# - Score: 3.00/3.00
# - Lines: 13
# - Complexity: 2
# - Structure: for_only
# - LineRelevance: 1.00
# - InputRelevance: 1.00
# - LineCoverage: 1.00
# - OutputVariability: 0.58
# - Seed: 47

def generated_func(arg0: int, arg1: int, arg2: int) -> int:
    var0 = arg2
    for i0 in range(abs(arg1) + 1):
        var1 = i0 + 8
        var0 //= abs(var1 + 4) % 2 + 1
    var0 *= (arg0 * arg0) % 4 + 3
    var0 //= max(2, abs(arg1) % 5)
    var0 //= 5
    var0 += arg2 + 5
    var0 -= arg0 - -6
    var0 += arg1 // 3
    var0 //= 2
    var0 *= (arg2 * arg2) % 2 + 3
    return var0