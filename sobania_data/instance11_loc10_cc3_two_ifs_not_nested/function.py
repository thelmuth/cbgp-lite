# Metrics:
# - Score: 3.00/3.00
# - Lines: 10
# - Complexity: 3
# - Structure: two_ifs_not_nested
# - LineRelevance: 1.00
# - InputRelevance: 1.00
# - LineCoverage: 1.00
# - OutputVariability: 0.57
# - Seed: 159

def generated_func(arg0: int, arg1: int, arg2: int) -> int:
    var0 = abs(arg0)
    if arg1 == 6:
        var0 *= (arg2 * arg2) % 3 + 3
    else:
        var0 = abs(arg2)
    var2 = 1
    if arg1 >= -6:
        var0 += abs(var2)
        var0 += arg0 * 5
    return var0