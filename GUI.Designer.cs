namespace Translator
{
    partial class s
    {
        /// <summary>
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Windows Form Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            this.folderBrowserDialog1 = new System.Windows.Forms.FolderBrowserDialog();
            this.BtnSource = new System.Windows.Forms.Button();
            this.BoxSource = new System.Windows.Forms.TextBox();
            this.BoxDest = new System.Windows.Forms.TextBox();
            this.BtnDest = new System.Windows.Forms.Button();
            this.BtnRun = new System.Windows.Forms.Button();
            this.listBox1 = new System.Windows.Forms.RichTextBox();
            this.label1 = new System.Windows.Forms.Label();
            this.richTextBox1 = new System.Windows.Forms.RichTextBox();
            this.label2 = new System.Windows.Forms.Label();
            this.BoxPatch = new System.Windows.Forms.TextBox();
            this.BtnPatch = new System.Windows.Forms.Button();
            this.BoxOverride = new System.Windows.Forms.TextBox();
            this.BtnOverride = new System.Windows.Forms.Button();
            this.label3 = new System.Windows.Forms.Label();
            this.BoxThreads = new System.Windows.Forms.TextBox();
            this.label4 = new System.Windows.Forms.Label();
            this.BoxThreadingEnabled = new System.Windows.Forms.CheckBox();
            this.BoxIL = new System.Windows.Forms.TextBox();
            this.ButtonIL = new System.Windows.Forms.Button();
            this.BoxWriteIL = new System.Windows.Forms.CheckBox();
            this.label5 = new System.Windows.Forms.Label();
            this.BoxReadIL = new System.Windows.Forms.CheckBox();
            this.label6 = new System.Windows.Forms.Label();
            this.BoxGenerateProjects = new System.Windows.Forms.CheckBox();
            this.label7 = new System.Windows.Forms.Label();
            this.BoxTemplate = new System.Windows.Forms.TextBox();
            this.button1 = new System.Windows.Forms.Button();
            this.BoxGroupProj = new System.Windows.Forms.TextBox();
            this.ButtonGroupProj = new System.Windows.Forms.Button();
            this.richTextBox2 = new System.Windows.Forms.RichTextBox();
            this.label8 = new System.Windows.Forms.Label();
            this.testBtn = new System.Windows.Forms.Button();
            this.checkLogFileNotFound = new System.Windows.Forms.CheckBox();
            this.groupBox1 = new System.Windows.Forms.GroupBox();
            this.checkLibs = new System.Windows.Forms.CheckBox();
            this.checkApps = new System.Windows.Forms.CheckBox();
            this.checkPlugins = new System.Windows.Forms.CheckBox();
            this.groupBox1.SuspendLayout();
            this.SuspendLayout();
            // 
            // BtnSource
            // 
            this.BtnSource.Location = new System.Drawing.Point(3, 9);
            this.BtnSource.Name = "BtnSource";
            this.BtnSource.Size = new System.Drawing.Size(85, 23);
            this.BtnSource.TabIndex = 0;
            this.BtnSource.Text = "Input Files";
            this.BtnSource.UseVisualStyleBackColor = true;
            this.BtnSource.Click += new System.EventHandler(this.BtnSource_Click);
            // 
            // BoxSource
            // 
            this.BoxSource.Location = new System.Drawing.Point(93, 12);
            this.BoxSource.Name = "BoxSource";
            this.BoxSource.Size = new System.Drawing.Size(379, 20);
            this.BoxSource.TabIndex = 1;
            // 
            // BoxDest
            // 
            this.BoxDest.Location = new System.Drawing.Point(93, 48);
            this.BoxDest.Name = "BoxDest";
            this.BoxDest.Size = new System.Drawing.Size(379, 20);
            this.BoxDest.TabIndex = 3;
            // 
            // BtnDest
            // 
            this.BtnDest.Location = new System.Drawing.Point(3, 45);
            this.BtnDest.Name = "BtnDest";
            this.BtnDest.Size = new System.Drawing.Size(85, 23);
            this.BtnDest.TabIndex = 2;
            this.BtnDest.Text = "Output Files";
            this.BtnDest.UseVisualStyleBackColor = true;
            this.BtnDest.Click += new System.EventHandler(this.BtnDest_Click);
            // 
            // BtnRun
            // 
            this.BtnRun.Location = new System.Drawing.Point(364, 706);
            this.BtnRun.Name = "BtnRun";
            this.BtnRun.Size = new System.Drawing.Size(75, 23);
            this.BtnRun.TabIndex = 4;
            this.BtnRun.Text = "Run";
            this.BtnRun.UseVisualStyleBackColor = true;
            this.BtnRun.Click += new System.EventHandler(this.BtnRun_Click);
            // 
            // listBox1
            // 
            this.listBox1.Location = new System.Drawing.Point(482, 249);
            this.listBox1.Name = "listBox1";
            this.listBox1.Size = new System.Drawing.Size(551, 450);
            this.listBox1.TabIndex = 5;
            this.listBox1.Text = "";
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Font = new System.Drawing.Font("Microsoft Sans Serif", 10F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label1.Location = new System.Drawing.Point(11, 229);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(353, 17);
            this.label1.TabIndex = 7;
            this.label1.Text = "Standard Delphi references and C# Equivalents";
            this.label1.Click += new System.EventHandler(this.label1_Click);
            // 
            // richTextBox1
            // 
            this.richTextBox1.Location = new System.Drawing.Point(11, 249);
            this.richTextBox1.Name = "richTextBox1";
            this.richTextBox1.Size = new System.Drawing.Size(460, 245);
            this.richTextBox1.TabIndex = 8;
            this.richTextBox1.Text = "";
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Font = new System.Drawing.Font("Microsoft Sans Serif", 10F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label2.Location = new System.Drawing.Point(478, 228);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(35, 17);
            this.label2.TabIndex = 9;
            this.label2.Text = "Log";
            this.label2.Click += new System.EventHandler(this.label2_Click);
            // 
            // BoxPatch
            // 
            this.BoxPatch.Location = new System.Drawing.Point(93, 123);
            this.BoxPatch.Name = "BoxPatch";
            this.BoxPatch.Size = new System.Drawing.Size(379, 20);
            this.BoxPatch.TabIndex = 11;
            // 
            // BtnPatch
            // 
            this.BtnPatch.Location = new System.Drawing.Point(3, 120);
            this.BtnPatch.Name = "BtnPatch";
            this.BtnPatch.Size = new System.Drawing.Size(85, 23);
            this.BtnPatch.TabIndex = 10;
            this.BtnPatch.Text = "Patch Files";
            this.BtnPatch.UseVisualStyleBackColor = true;
            this.BtnPatch.Click += new System.EventHandler(this.BtnPatch_Click);
            // 
            // BoxOverride
            // 
            this.BoxOverride.Location = new System.Drawing.Point(572, 15);
            this.BoxOverride.Name = "BoxOverride";
            this.BoxOverride.Size = new System.Drawing.Size(461, 20);
            this.BoxOverride.TabIndex = 13;
            // 
            // BtnOverride
            // 
            this.BtnOverride.Location = new System.Drawing.Point(482, 12);
            this.BtnOverride.Name = "BtnOverride";
            this.BtnOverride.Size = new System.Drawing.Size(85, 23);
            this.BtnOverride.TabIndex = 12;
            this.BtnOverride.Text = "Override Files";
            this.BtnOverride.UseVisualStyleBackColor = true;
            this.BtnOverride.Click += new System.EventHandler(this.BtnOverride_Click);
            // 
            // label3
            // 
            this.label3.AutoSize = true;
            this.label3.Location = new System.Drawing.Point(100, 193);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(46, 13);
            this.label3.TabIndex = 14;
            this.label3.Text = "Threads";
            this.label3.Click += new System.EventHandler(this.label3_Click);
            // 
            // BoxThreads
            // 
            this.BoxThreads.Location = new System.Drawing.Point(155, 190);
            this.BoxThreads.Name = "BoxThreads";
            this.BoxThreads.Size = new System.Drawing.Size(15, 20);
            this.BoxThreads.TabIndex = 15;
            this.BoxThreads.Text = "8";
            // 
            // label4
            // 
            this.label4.AutoSize = true;
            this.label4.Location = new System.Drawing.Point(9, 193);
            this.label4.Name = "label4";
            this.label4.Size = new System.Drawing.Size(55, 13);
            this.label4.TabIndex = 16;
            this.label4.Text = "Threading";
            // 
            // BoxThreadingEnabled
            // 
            this.BoxThreadingEnabled.AutoSize = true;
            this.BoxThreadingEnabled.Checked = true;
            this.BoxThreadingEnabled.CheckState = System.Windows.Forms.CheckState.Checked;
            this.BoxThreadingEnabled.Location = new System.Drawing.Point(73, 193);
            this.BoxThreadingEnabled.Name = "BoxThreadingEnabled";
            this.BoxThreadingEnabled.Size = new System.Drawing.Size(15, 14);
            this.BoxThreadingEnabled.TabIndex = 17;
            this.BoxThreadingEnabled.UseVisualStyleBackColor = true;
            this.BoxThreadingEnabled.CheckedChanged += new System.EventHandler(this.BoxThreadingEnabled_CheckedChanged);
            // 
            // BoxIL
            // 
            this.BoxIL.Location = new System.Drawing.Point(572, 51);
            this.BoxIL.Name = "BoxIL";
            this.BoxIL.Size = new System.Drawing.Size(461, 20);
            this.BoxIL.TabIndex = 19;
            // 
            // ButtonIL
            // 
            this.ButtonIL.Location = new System.Drawing.Point(482, 48);
            this.ButtonIL.Name = "ButtonIL";
            this.ButtonIL.Size = new System.Drawing.Size(85, 23);
            this.ButtonIL.TabIndex = 18;
            this.ButtonIL.Text = "IL Folder";
            this.ButtonIL.UseVisualStyleBackColor = true;
            // 
            // BoxWriteIL
            // 
            this.BoxWriteIL.AutoSize = true;
            this.BoxWriteIL.Checked = true;
            this.BoxWriteIL.CheckState = System.Windows.Forms.CheckState.Checked;
            this.BoxWriteIL.Location = new System.Drawing.Point(235, 193);
            this.BoxWriteIL.Name = "BoxWriteIL";
            this.BoxWriteIL.Size = new System.Drawing.Size(15, 14);
            this.BoxWriteIL.TabIndex = 21;
            this.BoxWriteIL.UseVisualStyleBackColor = true;
            // 
            // label5
            // 
            this.label5.AutoSize = true;
            this.label5.Location = new System.Drawing.Point(185, 194);
            this.label5.Name = "label5";
            this.label5.Size = new System.Drawing.Size(44, 13);
            this.label5.TabIndex = 20;
            this.label5.Text = "Write IL";
            // 
            // BoxReadIL
            // 
            this.BoxReadIL.AutoSize = true;
            this.BoxReadIL.Checked = true;
            this.BoxReadIL.CheckState = System.Windows.Forms.CheckState.Checked;
            this.BoxReadIL.Location = new System.Drawing.Point(308, 193);
            this.BoxReadIL.Name = "BoxReadIL";
            this.BoxReadIL.Size = new System.Drawing.Size(15, 14);
            this.BoxReadIL.TabIndex = 23;
            this.BoxReadIL.UseVisualStyleBackColor = true;
            // 
            // label6
            // 
            this.label6.AutoSize = true;
            this.label6.Location = new System.Drawing.Point(257, 194);
            this.label6.Name = "label6";
            this.label6.Size = new System.Drawing.Size(45, 13);
            this.label6.TabIndex = 22;
            this.label6.Text = "Read IL";
            // 
            // BoxGenerateProjects
            // 
            this.BoxGenerateProjects.AutoSize = true;
            this.BoxGenerateProjects.Checked = true;
            this.BoxGenerateProjects.CheckState = System.Windows.Forms.CheckState.Checked;
            this.BoxGenerateProjects.Location = new System.Drawing.Point(415, 193);
            this.BoxGenerateProjects.Name = "BoxGenerateProjects";
            this.BoxGenerateProjects.Size = new System.Drawing.Size(15, 14);
            this.BoxGenerateProjects.TabIndex = 25;
            this.BoxGenerateProjects.UseVisualStyleBackColor = true;
            // 
            // label7
            // 
            this.label7.AutoSize = true;
            this.label7.Location = new System.Drawing.Point(336, 193);
            this.label7.Name = "label7";
            this.label7.Size = new System.Drawing.Size(73, 13);
            this.label7.TabIndex = 24;
            this.label7.Text = "Convert .dproj";
            // 
            // BoxTemplate
            // 
            this.BoxTemplate.Location = new System.Drawing.Point(571, 88);
            this.BoxTemplate.Name = "BoxTemplate";
            this.BoxTemplate.Size = new System.Drawing.Size(461, 20);
            this.BoxTemplate.TabIndex = 27;
            // 
            // button1
            // 
            this.button1.Location = new System.Drawing.Point(481, 85);
            this.button1.Name = "button1";
            this.button1.Size = new System.Drawing.Size(85, 23);
            this.button1.TabIndex = 26;
            this.button1.Text = "Templates";
            this.button1.UseVisualStyleBackColor = true;
            // 
            // BoxGroupProj
            // 
            this.BoxGroupProj.Location = new System.Drawing.Point(92, 85);
            this.BoxGroupProj.Name = "BoxGroupProj";
            this.BoxGroupProj.Size = new System.Drawing.Size(379, 20);
            this.BoxGroupProj.TabIndex = 29;
            // 
            // ButtonGroupProj
            // 
            this.ButtonGroupProj.Location = new System.Drawing.Point(2, 82);
            this.ButtonGroupProj.Name = "ButtonGroupProj";
            this.ButtonGroupProj.Size = new System.Drawing.Size(85, 23);
            this.ButtonGroupProj.TabIndex = 28;
            this.ButtonGroupProj.Text = "GroupProj";
            this.ButtonGroupProj.UseVisualStyleBackColor = true;
            // 
            // richTextBox2
            // 
            this.richTextBox2.Location = new System.Drawing.Point(11, 531);
            this.richTextBox2.Name = "richTextBox2";
            this.richTextBox2.Size = new System.Drawing.Size(460, 168);
            this.richTextBox2.TabIndex = 30;
            this.richTextBox2.Text = "";
            // 
            // label8
            // 
            this.label8.AutoSize = true;
            this.label8.Font = new System.Drawing.Font("Microsoft Sans Serif", 10F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label8.Location = new System.Drawing.Point(12, 511);
            this.label8.Name = "label8";
            this.label8.Size = new System.Drawing.Size(182, 17);
            this.label8.TabIndex = 31;
            this.label8.Text = "Delphi library ignore list";
            // 
            // testBtn
            // 
            this.testBtn.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.testBtn.Location = new System.Drawing.Point(380, 91);
            this.testBtn.Name = "testBtn";
            this.testBtn.Size = new System.Drawing.Size(75, 23);
            this.testBtn.TabIndex = 32;
            this.testBtn.Text = "Test";
            this.testBtn.UseVisualStyleBackColor = true;
            this.testBtn.Click += new System.EventHandler(this.testBtn_Click);
            // 
            // checkLogFileNotFound
            // 
            this.checkLogFileNotFound.AutoSize = true;
            this.checkLogFileNotFound.Location = new System.Drawing.Point(20, 19);
            this.checkLogFileNotFound.Name = "checkLogFileNotFound";
            this.checkLogFileNotFound.Size = new System.Drawing.Size(144, 17);
            this.checkLogFileNotFound.TabIndex = 33;
            this.checkLogFileNotFound.Text = "Log correct file not found";
            this.checkLogFileNotFound.UseVisualStyleBackColor = true;
            // 
            // groupBox1
            // 
            this.groupBox1.Controls.Add(this.checkPlugins);
            this.groupBox1.Controls.Add(this.checkApps);
            this.groupBox1.Controls.Add(this.checkLibs);
            this.groupBox1.Controls.Add(this.checkLogFileNotFound);
            this.groupBox1.Controls.Add(this.testBtn);
            this.groupBox1.Location = new System.Drawing.Point(571, 123);
            this.groupBox1.Name = "groupBox1";
            this.groupBox1.Size = new System.Drawing.Size(461, 120);
            this.groupBox1.TabIndex = 34;
            this.groupBox1.TabStop = false;
            this.groupBox1.Text = "Test";
            // 
            // checkLibs
            // 
            this.checkLibs.AutoSize = true;
            this.checkLibs.Checked = true;
            this.checkLibs.CheckState = System.Windows.Forms.CheckState.Checked;
            this.checkLibs.Location = new System.Drawing.Point(20, 42);
            this.checkLibs.Name = "checkLibs";
            this.checkLibs.Size = new System.Drawing.Size(69, 17);
            this.checkLibs.TabIndex = 34;
            this.checkLibs.Text = "Test Libs";
            this.checkLibs.UseVisualStyleBackColor = true;
            // 
            // checkApps
            // 
            this.checkApps.AutoSize = true;
            this.checkApps.Checked = true;
            this.checkApps.CheckState = System.Windows.Forms.CheckState.Checked;
            this.checkApps.Location = new System.Drawing.Point(20, 65);
            this.checkApps.Name = "checkApps";
            this.checkApps.Size = new System.Drawing.Size(74, 17);
            this.checkApps.TabIndex = 35;
            this.checkApps.Text = "Test Apps";
            this.checkApps.UseVisualStyleBackColor = true;
            // 
            // checkPlugins
            // 
            this.checkPlugins.AutoSize = true;
            this.checkPlugins.Checked = true;
            this.checkPlugins.CheckState = System.Windows.Forms.CheckState.Checked;
            this.checkPlugins.Location = new System.Drawing.Point(20, 88);
            this.checkPlugins.Name = "checkPlugins";
            this.checkPlugins.Size = new System.Drawing.Size(84, 17);
            this.checkPlugins.TabIndex = 36;
            this.checkPlugins.Text = "Test Plugins";
            this.checkPlugins.UseVisualStyleBackColor = true;
            // 
            // s
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(1045, 741);
            this.Controls.Add(this.groupBox1);
            this.Controls.Add(this.label8);
            this.Controls.Add(this.richTextBox2);
            this.Controls.Add(this.BoxGroupProj);
            this.Controls.Add(this.ButtonGroupProj);
            this.Controls.Add(this.BoxTemplate);
            this.Controls.Add(this.button1);
            this.Controls.Add(this.BoxGenerateProjects);
            this.Controls.Add(this.label7);
            this.Controls.Add(this.BoxReadIL);
            this.Controls.Add(this.label6);
            this.Controls.Add(this.BoxWriteIL);
            this.Controls.Add(this.label5);
            this.Controls.Add(this.BoxIL);
            this.Controls.Add(this.ButtonIL);
            this.Controls.Add(this.BoxThreadingEnabled);
            this.Controls.Add(this.label4);
            this.Controls.Add(this.BoxThreads);
            this.Controls.Add(this.label3);
            this.Controls.Add(this.BoxOverride);
            this.Controls.Add(this.BtnOverride);
            this.Controls.Add(this.BoxPatch);
            this.Controls.Add(this.BtnPatch);
            this.Controls.Add(this.label2);
            this.Controls.Add(this.richTextBox1);
            this.Controls.Add(this.label1);
            this.Controls.Add(this.listBox1);
            this.Controls.Add(this.BtnRun);
            this.Controls.Add(this.BoxDest);
            this.Controls.Add(this.BtnDest);
            this.Controls.Add(this.BoxSource);
            this.Controls.Add(this.BtnSource);
            this.Name = "s";
            this.Text = "Delphi2CS";
            this.Load += new System.EventHandler(this.GUI_Load);
            this.groupBox1.ResumeLayout(false);
            this.groupBox1.PerformLayout();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.FolderBrowserDialog folderBrowserDialog1;
        private System.Windows.Forms.Button BtnSource;
        private System.Windows.Forms.TextBox BoxSource;
        private System.Windows.Forms.TextBox BoxDest;
        private System.Windows.Forms.Button BtnDest;
        private System.Windows.Forms.Button BtnRun;
        private System.Windows.Forms.RichTextBox listBox1;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.RichTextBox richTextBox1;
        private System.Windows.Forms.Label label2;
        private System.Windows.Forms.TextBox BoxPatch;
        private System.Windows.Forms.Button BtnPatch;
        private System.Windows.Forms.TextBox BoxOverride;
        private System.Windows.Forms.Button BtnOverride;
        private System.Windows.Forms.Label label3;
        private System.Windows.Forms.TextBox BoxThreads;
        private System.Windows.Forms.Label label4;
        private System.Windows.Forms.CheckBox BoxThreadingEnabled;
        private System.Windows.Forms.TextBox BoxIL;
        private System.Windows.Forms.Button ButtonIL;
        private System.Windows.Forms.CheckBox BoxWriteIL;
        private System.Windows.Forms.Label label5;
        private System.Windows.Forms.CheckBox BoxReadIL;
        private System.Windows.Forms.Label label6;
        private System.Windows.Forms.CheckBox BoxGenerateProjects;
        private System.Windows.Forms.Label label7;
        private System.Windows.Forms.TextBox BoxTemplate;
        private System.Windows.Forms.Button button1;
        private System.Windows.Forms.TextBox BoxGroupProj;
        private System.Windows.Forms.Button ButtonGroupProj;
        private System.Windows.Forms.RichTextBox richTextBox2;
        private System.Windows.Forms.Label label8;
        private System.Windows.Forms.Button testBtn;
        private System.Windows.Forms.CheckBox checkLogFileNotFound;
        private System.Windows.Forms.GroupBox groupBox1;
        private System.Windows.Forms.CheckBox checkLibs;
        private System.Windows.Forms.CheckBox checkApps;
        private System.Windows.Forms.CheckBox checkPlugins;

    }
}

