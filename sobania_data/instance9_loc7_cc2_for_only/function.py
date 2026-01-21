# Metrics:
# - Score: 3.00/3.00
# - Lines: 7
# - Complexity: 2
# - Structure: for_only
# - LineRelevance: 1.00
# - InputRelevance: 1.00
# - LineCoverage: 1.00
# - OutputVariability: 0.69
# - Seed: 36

def generated_func(arg0: int, arg1: int, arg2: int) -> int:
    var0 = arg0
    for i0 in range(abs(arg2) + 1):
        var0 = var0 + i0 * 2
    var1 = arg1 * arg0
    var2 = abs(arg2)
    var0 //= abs(var2) % 2 + 2
    return min(var1, var0)