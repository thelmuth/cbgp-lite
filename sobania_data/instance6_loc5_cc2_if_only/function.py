# Metrics:
# - Score: 3.00/3.00
# - Lines: 5
# - Complexity: 2
# - Structure: if_only
# - LineRelevance: 1.00
# - InputRelevance: 1.00
# - LineCoverage: 1.00
# - OutputVariability: 0.53
# - Seed: 20

def generated_func(arg0: int, arg1: int, arg2: int) -> int:
    var0 = arg2
    if arg0 < -2:
        var0 //= abs(arg1 + 9) % 2 + 3
        var0 = abs(var0)
    return var0 * 2