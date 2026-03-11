% 指定文件夹路径
folder_path = '/Users/holland/Documents/GitHub/Portfolio01/data';

% 获取所有.mat文件
mat_files = dir(fullfile(folder_path, '*.mat'));

% 遍历所有文件
for i = 1:length(mat_files)
    % 构建完整文件路径
    file_path = fullfile(folder_path, mat_files(i).name);
    % 加载文件
    data = load(file_path);
    
    % 检查是否存在p结构体和date变量
    if isfield(data, 'p') && isfield(data.p, 'Date')
        % 删除date字段
        data.p = rmfield(data.p, 'Date');
        
        % 保存回原文件
        save(file_path, '-struct', 'data');
    end
end
