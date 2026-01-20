# Metrics:
# - Score: 3.00/3.00
# - Lines: 10
# - Complexity: 3
# - Structure: two_ifs_not_nested
# - LineRelevance: 1.00
# - InputRelevance: 1.00
# - LineCoverage: 1.00
# - OutputVariability: 0.59
# - Seed: 56

def generated_func(arg0: int, arg1: int, arg2: int) -> int:
    var0 = 2
    if arg2 < 2:
        var0 -= arg0 + 7
        var0 = var0 + arg2
    var0 = var0 + arg1
    if arg0 > 9:
        var0 = abs(arg1)
    else:
        var0 = var0 * arg2
    return max(var0, arg1)