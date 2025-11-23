/**
 * @file asteroid_filter.cpp
 * @brief Implementation of advanced asteroid filtering system
 * @author IOccultCalc Team
 * @date 2025-11-22
 */

#include "ioccultcalc/asteroid_filter.h"
#include <algorithm>
#include <sstream>
#include <stdexcept>
#include <cctype>

namespace ioccultcalc {

// ============================================================================
// Helper functions for string parsing
// ============================================================================

static std::string trim(const std::string& str) {
    size_t start = str.find_first_not_of(" \t\n\r");
    if (start == std::string::npos) return "";
    size_t end = str.find_last_not_of(" \t\n\r");
    return str.substr(start, end - start + 1);
}

static std::vector<std::string> split(const std::string& str, char delimiter) {
    std::vector<std::string> tokens;
    std::stringstream ss(str);
    std::string token;
    while (std::getline(ss, token, delimiter)) {
        tokens.push_back(trim(token));
    }
    return tokens;
}

static std::string toLower(const std::string& str) {
    std::string result = str;
    std::transform(result.begin(), result.end(), result.begin(),
                   [](unsigned char c) { return std::tolower(c); });
    return result;
}

// ============================================================================
// FilterCondition implementation
// ============================================================================

FilterCondition::FilterCondition(const std::string& field, ComparisonOp op, const std::string& value)
    : field_(field), operator_(op), value_(value) {}

bool FilterCondition::evaluate(const AsteroidProperties& props) const {
    // Extract field value
    double fieldValue = 0.0;
    std::string fieldStr = "";
    bool isNumeric = true;

    if (field_ == "number") {
        fieldValue = static_cast<double>(props.number);
    } else if (field_ == "diameter") {
        fieldValue = props.diameter;
    } else if (field_ == "H") {
        fieldValue = props.H;
    } else if (field_ == "albedo") {
        fieldValue = props.albedo;
    } else if (field_ == "a") {
        fieldValue = props.a;
    } else if (field_ == "e") {
        fieldValue = props.e;
    } else if (field_ == "i") {
        fieldValue = props.i;
    } else if (field_ == "rotation_period") {
        fieldValue = props.rotation_period;
    } else if (field_ == "orbit_class") {
        fieldStr = props.orbit_class;
        isNumeric = false;
    } else if (field_ == "spectral_type") {
        fieldStr = props.spectral_type;
        isNumeric = false;
    } else if (field_ == "designation") {
        fieldStr = props.designation;
        isNumeric = false;
    } else if (field_ == "name") {
        fieldStr = props.name;
        isNumeric = false;
    } else {
        throw std::invalid_argument("Unknown field: " + field_);
    }

    // Evaluate based on operator
    if (isNumeric) {
        return evaluateNumeric(fieldValue);
    } else {
        return evaluateString(fieldStr);
    }
}

bool FilterCondition::evaluateNumeric(double fieldValue) const {
    double compareValue;
    try {
        compareValue = std::stod(value_);
    } catch (...) {
        return false;
    }

    switch (operator_) {
        case ComparisonOp::EQUAL:
            return std::abs(fieldValue - compareValue) < 1e-6;
        case ComparisonOp::NOT_EQUAL:
            return std::abs(fieldValue - compareValue) >= 1e-6;
        case ComparisonOp::LESS_THAN:
            return fieldValue < compareValue;
        case ComparisonOp::LESS_EQUAL:
            return fieldValue <= compareValue;
        case ComparisonOp::GREATER_THAN:
            return fieldValue > compareValue;
        case ComparisonOp::GREATER_EQUAL:
            return fieldValue >= compareValue;
        case ComparisonOp::BETWEEN: {
            // Format: "value1 and value2"
            auto parts = split(value_, ' ');
            if (parts.size() >= 3) {
                double v1 = std::stod(parts[0]);
                double v2 = std::stod(parts[2]);
                return fieldValue >= v1 && fieldValue <= v2;
            }
            return false;
        }
        default:
            return false;
    }
}

bool FilterCondition::evaluateString(const std::string& fieldValue) const {
    std::string fieldLower = toLower(fieldValue);
    std::string valueLower = toLower(value_);

    switch (operator_) {
        case ComparisonOp::EQUAL:
            return fieldLower == valueLower;
        case ComparisonOp::NOT_EQUAL:
            return fieldLower != valueLower;
        case ComparisonOp::IN: {
            // Format: "[val1, val2, val3]" or "val1, val2, val3"
            std::string listStr = value_;
            // Remove brackets
            listStr.erase(std::remove(listStr.begin(), listStr.end(), '['), listStr.end());
            listStr.erase(std::remove(listStr.begin(), listStr.end(), ']'), listStr.end());
            listStr.erase(std::remove(listStr.begin(), listStr.end(), '"'), listStr.end());
            
            auto values = split(listStr, ',');
            for (const auto& v : values) {
                if (toLower(trim(v)) == fieldLower) {
                    return true;
                }
            }
            return false;
        }
        case ComparisonOp::NOT_IN: {
            // Same as IN but inverted
            std::string listStr = value_;
            listStr.erase(std::remove(listStr.begin(), listStr.end(), '['), listStr.end());
            listStr.erase(std::remove(listStr.begin(), listStr.end(), ']'), listStr.end());
            listStr.erase(std::remove(listStr.begin(), listStr.end(), '"'), listStr.end());
            
            auto values = split(listStr, ',');
            for (const auto& v : values) {
                if (toLower(trim(v)) == fieldLower) {
                    return false;
                }
            }
            return true;
        }
        case ComparisonOp::LIKE: {
            // Simple wildcard matching with *
            std::string pattern = valueLower;
            if (pattern.find('*') != std::string::npos) {
                // Replace * with .*  for regex-like behavior (simplified)
                size_t pos = 0;
                while ((pos = pattern.find('*', pos)) != std::string::npos) {
                    pattern.replace(pos, 1, ".*");
                    pos += 2;
                }
            }
            return fieldLower.find(valueLower.substr(0, valueLower.find('*'))) != std::string::npos;
        }
        default:
            return false;
    }
}

std::string FilterCondition::toString() const {
    std::string opStr;
    switch (operator_) {
        case ComparisonOp::EQUAL: opStr = "=="; break;
        case ComparisonOp::NOT_EQUAL: opStr = "!="; break;
        case ComparisonOp::LESS_THAN: opStr = "<"; break;
        case ComparisonOp::LESS_EQUAL: opStr = "<="; break;
        case ComparisonOp::GREATER_THAN: opStr = ">"; break;
        case ComparisonOp::GREATER_EQUAL: opStr = ">="; break;
        case ComparisonOp::IN: opStr = "in"; break;
        case ComparisonOp::NOT_IN: opStr = "not in"; break;
        case ComparisonOp::BETWEEN: opStr = "between"; break;
        case ComparisonOp::LIKE: opStr = "like"; break;
    }
    return field_ + " " + opStr + " " + value_;
}

FilterCondition FilterCondition::parse(const std::string& condition) {
    std::string trimmed = trim(condition);
    
    // Try to find operator
    ComparisonOp op;
    size_t opPos = std::string::npos;
    std::string opStr;
    
    // Check for multi-character operators first
    if ((opPos = trimmed.find(" not in ")) != std::string::npos) {
        op = ComparisonOp::NOT_IN;
        opStr = " not in ";
    } else if ((opPos = trimmed.find(" in ")) != std::string::npos) {
        op = ComparisonOp::IN;
        opStr = " in ";
    } else if ((opPos = trimmed.find(" between ")) != std::string::npos) {
        op = ComparisonOp::BETWEEN;
        opStr = " between ";
    } else if ((opPos = trimmed.find(" like ")) != std::string::npos) {
        op = ComparisonOp::LIKE;
        opStr = " like ";
    } else if ((opPos = trimmed.find(">=")) != std::string::npos) {
        op = ComparisonOp::GREATER_EQUAL;
        opStr = ">=";
    } else if ((opPos = trimmed.find("<=")) != std::string::npos) {
        op = ComparisonOp::LESS_EQUAL;
        opStr = "<=";
    } else if ((opPos = trimmed.find("!=")) != std::string::npos) {
        op = ComparisonOp::NOT_EQUAL;
        opStr = "!=";
    } else if ((opPos = trimmed.find("==")) != std::string::npos) {
        op = ComparisonOp::EQUAL;
        opStr = "==";
    } else if ((opPos = trimmed.find("=")) != std::string::npos) {
        op = ComparisonOp::EQUAL;
        opStr = "=";
    } else if ((opPos = trimmed.find(">")) != std::string::npos) {
        op = ComparisonOp::GREATER_THAN;
        opStr = ">";
    } else if ((opPos = trimmed.find("<")) != std::string::npos) {
        op = ComparisonOp::LESS_THAN;
        opStr = "<";
    } else {
        throw std::invalid_argument("No operator found in condition: " + condition);
    }
    
    std::string field = trim(trimmed.substr(0, opPos));
    std::string value = trim(trimmed.substr(opPos + opStr.length()));
    
    return FilterCondition(field, op, value);
}

// ============================================================================
// AsteroidRange implementation
// ============================================================================

AsteroidRange::AsteroidRange() : useExplicitList_(false) {}

bool AsteroidRange::matches(const AsteroidProperties& props) const {
    // Check if in range or explicit list
    if (useExplicitList_) {
        if (std::find(explicitList_.begin(), explicitList_.end(), props.number) == explicitList_.end()) {
            return false;
        }
    } else {
        if (props.number < from_ || props.number > to_) {
            return false;
        }
    }
    
    // Check WHERE conditions (all must be true - AND logic)
    for (const auto& condition : whereConditions_) {
        if (!condition.evaluate(props)) {
            return false;
        }
    }
    
    // Check WHERENOT conditions (all must be false - OR exclusion)
    for (const auto& condition : whereNotConditions_) {
        if (condition.evaluate(props)) {
            return false;
        }
    }
    
    return true;
}

std::vector<int> AsteroidRange::getAsteroidList() const {
    if (useExplicitList_) {
        return explicitList_;
    }
    
    // Generate range
    std::vector<int> list;
    for (int i = from_; i <= to_; ++i) {
        list.push_back(i);
    }
    return list;
}

void AsteroidRange::setRange(int from, int to) {
    from_ = from;
    to_ = to;
    useExplicitList_ = false;
}

void AsteroidRange::setExplicitList(const std::vector<int>& list) {
    explicitList_ = list;
    useExplicitList_ = true;
}

void AsteroidRange::addWhereCondition(const std::string& condition) {
    whereConditions_.push_back(FilterCondition::parse(condition));
}

void AsteroidRange::addWhereCondition(const FilterCondition& condition) {
    whereConditions_.push_back(condition);
}

void AsteroidRange::addWhereNotCondition(const std::string& condition) {
    whereNotConditions_.push_back(FilterCondition::parse(condition));
}

void AsteroidRange::addWhereNotCondition(const FilterCondition& condition) {
    whereNotConditions_.push_back(condition);
}

std::string AsteroidRange::toString() const {
    std::stringstream ss;
    
    if (useExplicitList_) {
        ss << "Explicit list: [";
        for (size_t i = 0; i < explicitList_.size() && i < 10; ++i) {
            if (i > 0) ss << ", ";
            ss << explicitList_[i];
        }
        if (explicitList_.size() > 10) ss << ", ...";
        ss << "]";
    } else {
        ss << "Range: " << from_ << " to " << to_;
    }
    
    if (!whereConditions_.empty()) {
        ss << "\nWHERE:\n";
        for (const auto& cond : whereConditions_) {
            ss << "  - " << cond.toString() << "\n";
        }
    }
    
    if (!whereNotConditions_.empty()) {
        ss << "WHERENOT:\n";
        for (const auto& cond : whereNotConditions_) {
            ss << "  - " << cond.toString() << "\n";
        }
    }
    
    return ss.str();
}

// ============================================================================
// AsteroidRangeBuilder implementation
// ============================================================================

AsteroidRangeBuilder::AsteroidRangeBuilder() {
    range_.setRange(1, 100000); // Default range
}

AsteroidRangeBuilder& AsteroidRangeBuilder::from(int start) {
    from_ = start;
    useExplicitList_ = false;
    return *this;
}

AsteroidRangeBuilder& AsteroidRangeBuilder::to(int end) {
    to_ = end;
    useExplicitList_ = false;
    return *this;
}

AsteroidRangeBuilder& AsteroidRangeBuilder::explicitList(const std::vector<int>& list) {
    explicitList_ = list;
    useExplicitList_ = true;
    return *this;
}

AsteroidRangeBuilder& AsteroidRangeBuilder::where(const std::string& condition) {
    whereConditions_.push_back(condition);
    return *this;
}

AsteroidRangeBuilder& AsteroidRangeBuilder::whereNot(const std::string& condition) {
    whereNotConditions_.push_back(condition);
    return *this;
}

AsteroidRange AsteroidRangeBuilder::build() {
    if (useExplicitList_) {
        range_.setExplicitList(explicitList_);
    } else {
        range_.setRange(from_, to_);
    }
    
    for (const auto& condStr : whereConditions_) {
        range_.addWhereCondition(condStr);
    }
    
    for (const auto& condStr : whereNotConditions_) {
        range_.addWhereNotCondition(condStr);
    }
    
    return range_;
}

// ============================================================================
// AsteroidFilterPresets implementation
// ============================================================================

AsteroidRange AsteroidFilterPresets::largeMBA() {
    return AsteroidRangeBuilder()
        .from(1).to(100000)
        .where("diameter > 50")
        .where("H < 10")
        .where("orbit_class in [MBA, IMB, OMB]")
        .where("a > 2.2")
        .where("a < 3.2")
        .build();
}

AsteroidRange AsteroidFilterPresets::nearEarth() {
    return AsteroidRangeBuilder()
        .from(1).to(100000)
        .where("orbit_class in [NEA, Aten, Apollo, Amor]")
        .where("a < 1.3")
        .where("diameter > 0.1")
        .build();
}

AsteroidRange AsteroidFilterPresets::potentiallyHazardous() {
    return AsteroidRangeBuilder()
        .from(1).to(100000)
        .where("orbit_class in [NEA, Aten, Apollo, Amor, PHA]")
        .where("diameter > 0.14")
        .where("H < 22")
        .build();
}

AsteroidRange AsteroidFilterPresets::trojans() {
    return AsteroidRangeBuilder()
        .from(588).to(100000)
        .where("orbit_class in [L4, L5, Trojan]")
        .where("a > 4.5")
        .where("a < 5.5")
        .where("diameter > 20")
        .build();
}

AsteroidRange AsteroidFilterPresets::centaurs() {
    return AsteroidRangeBuilder()
        .from(1).to(100000)
        .where("orbit_class in [Centaur]")
        .where("a > 5.5")
        .where("a < 30")
        .where("diameter > 50")
        .build();
}

AsteroidRange AsteroidFilterPresets::darkAsteroids() {
    return AsteroidRangeBuilder()
        .from(1).to(100000)
        .where("albedo < 0.07")
        .where("spectral_type in [C, P, D, B]")
        .where("diameter > 30")
        .build();
}

AsteroidRange AsteroidFilterPresets::metallic() {
    return AsteroidRangeBuilder()
        .from(1).to(100000)
        .where("spectral_type in [M, E, X]")
        .where("albedo > 0.15")
        .where("diameter > 20")
        .build();
}

AsteroidRange AsteroidFilterPresets::historic() {
    return AsteroidRangeBuilder()
        .from(1).to(100)
        .build();
}

AsteroidRange AsteroidFilterPresets::fastRotators() {
    return AsteroidRangeBuilder()
        .from(1).to(100000)
        .where("rotation_period < 3")
        .where("diameter > 5")
        .where("diameter < 50")
        .build();
}

AsteroidRange AsteroidFilterPresets::binaryCandidates() {
    return AsteroidRangeBuilder()
        .from(1).to(100000)
        .where("diameter > 50")
        .where("spectral_type in [M, E, X]")
        .where("rotation_period > 2")
        .where("rotation_period < 5")
        .build();
}

} // namespace ioccultcalc
