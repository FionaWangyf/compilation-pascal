#include "common/setting/settings.hpp"
#include "common/log/log.hpp"
#include <cstdlib>
#include <unistd.h>
#include <cstring>

namespace common {

static Settings g_settings;

Settings &Settings::get_instance()
{
    return g_settings;
}

void Settings::parse_args(int argc, char *argv[])
{
    int opt;
    while((opt = getopt(argc,argv,"d:i:o:h:j:O:D")) != -1)
    {
        switch(opt)
        {
            case 'd':
                log_level = (atoi(optarg) > 4 || atoi(optarg) < 0) ? 0 : atoi(optarg);
                break;
            case 'i':
                input_file = optarg;
                break;
            case 'o':
                output_file = optarg;
                break;
            case 'h':
                say_help();
                exit(0);
                break;
            default:
                LOG_FATAL("未知操作: %c", opt);
                say_help();
                break;
        }
    }
}

void Settings::say_help()
{
    LOG_INFO("用法: ./pascc [选项]");
    LOG_INFO("选项:");
    LOG_INFO("  -d <等级>   设置日志等级(0-4: 最低-最高)");
    LOG_INFO("  -i <文件>   指定输入文件");
    LOG_INFO("  -o <文件>   指定输出文件");
    LOG_INFO("  -h          显示此帮助信息");
}

} // namespace common