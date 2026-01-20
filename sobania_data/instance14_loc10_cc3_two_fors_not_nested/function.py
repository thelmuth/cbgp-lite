# Metrics:
# - Score: 3.00/3.00
# - Lines: 10
# - Complexity: 3
# - Structure: two_fors_not_nested
# - LineRelevance: 1.00
# - InputRelevance: 1.00
# - LineCoverage: 1.00
# - OutputVariability: 0.79
# - Seed: 34

def generated_func(arg0: int, arg1: int, arg2: int) -> int:
    var0 = 1
    for i0 in range(abs(arg2) + 1):
        var1 = (i0 + 0.64) / 3.54
        var0 = max(var0, i0)
    var1 *= max(2, abs(arg0) % 4)
    for i1 in range(abs(arg1) + 1):
        var1 += (var0 + 1.33) / 11
        var1 = var1 - i1
    var0 = int(var1)
    return var0 + i1