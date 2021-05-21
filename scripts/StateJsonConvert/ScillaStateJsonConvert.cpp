#include <iostream>
#include <fstream>
#include <memory>
#include <unordered_map>
#include <jsoncpp/json/reader.h>
#include <jsoncpp/json/value.h>

#include <boost/program_options.hpp>

namespace po = boost::program_options;

void exitMsg(const std::string &Msg) {
  std::cerr << Msg;
  exit(EXIT_FAILURE);
}

void parseCLIArgs(int argc, char *argv[], po::variables_map &VM) {
  auto UsageString =
      "Usage: " + std::string(argv[0]) +
      " -c contract_info.json -s state.json"
      "\nSupported options";
  po::options_description Desc(UsageString);

  // clang-format off
  Desc.add_options()
    ("state,s", po::value<std::string>(), "Specify the JSON to use as initial state")
    ("contract-info,c", po::value<std::string>(), "Specify the contract info JSON from checker")
    ("help,h", "Print help message")
    ("version,v", "Print version")
  ;
  // clang-format on

  try {
    po::store(po::command_line_parser(argc, argv).options(Desc).run(), VM);
    po::notify(VM);
  } catch (std::exception &e) {
    std::cerr << e.what() << "\n";
    std::cerr << Desc << "\n";
    exit(EXIT_FAILURE);
  }

  if (VM.count("help")) {
    std::cerr << Desc << "\n";
    exit(EXIT_SUCCESS);
  }

  // Ensure that an input file is provided.
  if (!VM.count("state") || !VM.count("contract-info")) {
    std::cerr << "Missing mandatory command line arguments\n" << Desc << "\n";
    exit(EXIT_FAILURE);
  }
}

std::string readFile(const std::string &Filename) {
  std::ifstream IfsFile(Filename);
  std::string FileStr((std::istreambuf_iterator<char>(IfsFile)),
                      (std::istreambuf_iterator<char>()));
  return FileStr;
}

Json::Value parseJSONString(const std::string &JS) {
  Json::Value Ret;
  Json::CharReaderBuilder ReadBuilder;
  std::unique_ptr<Json::CharReader> Reader(ReadBuilder.newCharReader());
  std::string Error;
  try {
    Reader->parse(JS.c_str(), JS.c_str() + JS.length(), &Ret, &Error);
  } catch (const std::exception &e) {
    exitMsg(std::string(e.what()) + ": " + Error);
  }

  return Ret;
}

Json::Value parseJSONFile(const std::string &Filename) {

  return parseJSONString(readFile(Filename));
}

Json::Value convertValue(const Json::Value &IJ, int Depth) {
  if (Depth == 0) {
    return IJ;
  }

  if (IJ.isNull()) {
    return Json::arrayValue;
  }

  if (!IJ.isObject()) {
    exitMsg("Expected JSON object at depth " + std::to_string(Depth));
  }

  Json::Value OJ = Json::arrayValue;
  for (Json::Value::const_iterator Itr = IJ.begin();
       Itr != IJ.end(); Itr++)
  {
    Json::Value J = Json::objectValue;
    J["key"] = Itr.name();
    J["val"] = convertValue(*Itr, Depth - 1);
    OJ.append(J);
  }

  return OJ;  
}

int main(int argc, char *argv[])
{
  po::variables_map VM;
  parseCLIArgs(argc, argv, VM);

  // Process the contract info JSON first.
  std::string ContrInfoFilename = VM["contract-info"].as<std::string>();
  Json::Value ContrInfoJ = parseJSONFile(ContrInfoFilename);

  Json::Value &Fields = ContrInfoJ["contract_info"]["fields"];
  if (Fields.isNull()) {
    exitMsg("fields not found in " + ContrInfoFilename);
  }

  std::unordered_map<std::string, int> DepthMap;
  std::unordered_map<std::string, std::string> TypeMap;
  DepthMap["_balance"] = 0;
  TypeMap["_balance"] = "Uint128";
  
  for (const auto &Field : Fields) {
    if (!Field["vname"].isString() || !Field["type"].isString()
        || !Field["depth"].isInt())
    {
      exitMsg("Incorrect field entry.\n" + Field.toStyledString());
    }
    TypeMap[Field["vname"].asString()] = Field["type"].asString();
    DepthMap[Field["vname"].asString()] = Field["depth"].asInt();
  }

  std::string InputStateFilename = VM["state"].as<std::string>();
  Json::Value InputState = parseJSONFile(InputStateFilename);

  Json::Value OutputState = Json::arrayValue;
  if (!InputState.isObject()) {
    exitMsg("Expected input state to be a JSON object");
  }

  for (Json::Value::const_iterator Itr = InputState.begin();
       Itr != InputState.end(); Itr++)
  {
    const std::string &VName = Itr.name();
    const Json::Value &ValJ = *Itr;
    const auto &Type = TypeMap.find(VName);
    const auto &Depth = DepthMap.find(VName);
    if (Type == TypeMap.end() || Depth == DepthMap.end()) {
      exitMsg("Not found: type or depth for " + VName);
    }
    Json::Value OJ;
    OJ["vname"] = VName;
    OJ["type"] = Type->second;
    OJ["value"] = convertValue(ValJ, Depth->second);
    OutputState.append(OJ);
  }

  std::cout << OutputState.toStyledString();

  return 0;
}
