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
            this.BoxSource.Size = new System.Drawing.Size(347, 20);
            this.BoxSource.TabIndex = 1;
            // 
            // BoxDest
            // 
            this.BoxDest.Location = new System.Drawing.Point(93, 48);
            this.BoxDest.Name = "BoxDest";
            this.BoxDest.Size = new System.Drawing.Size(347, 20);
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
            this.BtnRun.Location = new System.Drawing.Point(364, 646);
            this.BtnRun.Name = "BtnRun";
            this.BtnRun.Size = new System.Drawing.Size(75, 23);
            this.BtnRun.TabIndex = 4;
            this.BtnRun.Text = "Run";
            this.BtnRun.UseVisualStyleBackColor = true;
            this.BtnRun.Click += new System.EventHandler(this.BtnRun_Click);
            // 
            // listBox1
            // 
            this.listBox1.Location = new System.Drawing.Point(12, 393);
            this.listBox1.Name = "listBox1";
            this.listBox1.Size = new System.Drawing.Size(427, 224);
            this.listBox1.TabIndex = 5;
            this.listBox1.Text = "";
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Font = new System.Drawing.Font("Microsoft Sans Serif", 10F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label1.Location = new System.Drawing.Point(12, 189);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(353, 17);
            this.label1.TabIndex = 7;
            this.label1.Text = "Standard Delphi references and C# Equivalents";
            this.label1.Click += new System.EventHandler(this.label1_Click);
            // 
            // richTextBox1
            // 
            this.richTextBox1.Location = new System.Drawing.Point(12, 214);
            this.richTextBox1.Name = "richTextBox1";
            this.richTextBox1.Size = new System.Drawing.Size(428, 139);
            this.richTextBox1.TabIndex = 8;
            this.richTextBox1.Text = "";
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Font = new System.Drawing.Font("Microsoft Sans Serif", 10F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label2.Location = new System.Drawing.Point(12, 365);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(35, 17);
            this.label2.TabIndex = 9;
            this.label2.Text = "Log";
            this.label2.Click += new System.EventHandler(this.label2_Click);
            // 
            // BoxPatch
            // 
            this.BoxPatch.Location = new System.Drawing.Point(93, 84);
            this.BoxPatch.Name = "BoxPatch";
            this.BoxPatch.Size = new System.Drawing.Size(347, 20);
            this.BoxPatch.TabIndex = 11;
            // 
            // BtnPatch
            // 
            this.BtnPatch.Location = new System.Drawing.Point(3, 81);
            this.BtnPatch.Name = "BtnPatch";
            this.BtnPatch.Size = new System.Drawing.Size(85, 23);
            this.BtnPatch.TabIndex = 10;
            this.BtnPatch.Text = "Patch Files";
            this.BtnPatch.UseVisualStyleBackColor = true;
            this.BtnPatch.Click += new System.EventHandler(this.BtnPatch_Click);
            // 
            // BoxOverride
            // 
            this.BoxOverride.Location = new System.Drawing.Point(93, 121);
            this.BoxOverride.Name = "BoxOverride";
            this.BoxOverride.Size = new System.Drawing.Size(347, 20);
            this.BoxOverride.TabIndex = 13;
            // 
            // BtnOverride
            // 
            this.BtnOverride.Location = new System.Drawing.Point(3, 118);
            this.BtnOverride.Name = "BtnOverride";
            this.BtnOverride.Size = new System.Drawing.Size(85, 23);
            this.BtnOverride.TabIndex = 12;
            this.BtnOverride.Text = "Override Files";
            this.BtnOverride.UseVisualStyleBackColor = true;
            this.BtnOverride.Click += new System.EventHandler(this.BtnOverride_Click);
            // 
            // s
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(456, 688);
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
            this.Text = "GUI";
            this.Load += new System.EventHandler(this.GUI_Load);
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

    }
}

