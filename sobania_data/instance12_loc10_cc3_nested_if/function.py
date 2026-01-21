# Metrics:
# - Score: 3.00/3.00
# - Lines: 10
# - Complexity: 3
# - Structure: nested_if
# - LineRelevance: 1.00
# - InputRelevance: 1.00
# - LineCoverage: 1.00
# - OutputVariability: 0.58
# - Seed: 302

def generated_func(arg0: int, arg1: int, arg2: int) -> int:
    var0 = 1
    if arg2 <= -5:
        var1 = arg2 % 10
        if arg1 > 4:
            var1 //= (arg1 * arg1) % 2 + 2
        else:
            var1 += arg2
        var0 += var1
    var0 -= abs(arg0)
    return var0