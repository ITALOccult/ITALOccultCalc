using System;
using System.ComponentModel;
using System.Drawing;
using System.IO;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult.Asteroid_Observations
{
	public class SetCompileFileNames : Form
	{
		private IContainer components;

		private TextBox txtFileName;

		private Label label1;

		private Label label2;

		private Label label3;

		private Label label22;

		private ListBox lstQualityFiles;

		private Button cmdAccept;

		private Button cmdCancel;

		public SetCompileFileNames()
		{
			InitializeComponent();
		}

		private void PopulateListOfQualityFiles()
		{
			lstQualityFiles.get_Items().Clear();
			FileInfo[] files = new DirectoryInfo(Utilities.AppPath + "\\Import_Export").GetFiles(ObservationsEditor.Quality_FileNameBase + "*.xml");
			if (files.Length >= 0)
			{
				FileInfo[] array = files;
				foreach (FileInfo obj in array)
				{
					int num = obj.Name.IndexOf(".xml", ObservationsEditor.Quality_FileNameBase.Length);
					string text = obj.Name.Substring(ObservationsEditor.Quality_FileNameBase.Length, num - ObservationsEditor.Quality_FileNameBase.Length);
					lstQualityFiles.get_Items().Add((object)text);
				}
			}
			else
			{
				lstQualityFiles.get_Items().Add((object)"None available");
			}
		}

		private void SetCompileFileNames_Load(object sender, EventArgs e)
		{
			((Control)txtFileName).set_Text(Settings.Default.QualityFileName);
			PopulateListOfQualityFiles();
		}

		private void cmdCancel_Click(object sender, EventArgs e)
		{
			((Form)this).set_DialogResult((DialogResult)2);
			((Form)this).Close();
			((Component)this).Dispose();
		}

		private void Accept_Click(object sender, EventArgs e)
		{
			Utilities.CheckForPipesAndNonASCII(((Control)txtFileName).get_Text().Trim(), "FileName", CheckForPipes: false, out var RevisedText);
			Settings.Default.QualityFileName = RevisedText;
			((Form)this).set_DialogResult((DialogResult)1);
			((Form)this).Close();
			((Component)this).Dispose();
		}

		protected override void Dispose(bool disposing)
		{
			if (disposing && components != null)
			{
				components.Dispose();
			}
			((Form)this).Dispose(disposing);
		}

		private void InitializeComponent()
		{
			//IL_0001: Unknown result type (might be due to invalid IL or missing references)
			//IL_000b: Expected O, but got Unknown
			//IL_000c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0016: Expected O, but got Unknown
			//IL_0017: Unknown result type (might be due to invalid IL or missing references)
			//IL_0021: Expected O, but got Unknown
			//IL_0022: Unknown result type (might be due to invalid IL or missing references)
			//IL_002c: Expected O, but got Unknown
			//IL_002d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0037: Expected O, but got Unknown
			//IL_0038: Unknown result type (might be due to invalid IL or missing references)
			//IL_0042: Expected O, but got Unknown
			//IL_0043: Unknown result type (might be due to invalid IL or missing references)
			//IL_004d: Expected O, but got Unknown
			//IL_004e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0058: Expected O, but got Unknown
			txtFileName = new TextBox();
			label1 = new Label();
			label2 = new Label();
			label3 = new Label();
			label22 = new Label();
			lstQualityFiles = new ListBox();
			cmdAccept = new Button();
			cmdCancel = new Button();
			((Control)this).SuspendLayout();
			((Control)txtFileName).set_Location(new Point(69, 37));
			((Control)txtFileName).set_Name("txtFileName");
			((Control)txtFileName).set_Size(new Size(102, 20));
			((Control)txtFileName).set_TabIndex(0);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label1).set_Location(new Point(0, 39));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(68, 17));
			((Control)label1).set_TabIndex(1);
			((Control)label1).set_Text("Quality_");
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label2).set_Location(new Point(173, 39));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(36, 17));
			((Control)label2).set_TabIndex(2);
			((Control)label2).set_Text(".xml");
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Location(new Point(83, 21));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(52, 13));
			((Control)label3).set_TabIndex(3);
			((Control)label3).set_Text("File name");
			((Control)label22).set_AutoSize(true);
			((Control)label22).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label22).set_Location(new Point(11, 69));
			((Control)label22).set_Name("label22");
			((Control)label22).set_Size(new Size(99, 17));
			((Control)label22).set_TabIndex(19);
			((Control)label22).set_Text("Existing files");
			label22.set_TextAlign(ContentAlignment.TopCenter);
			((Control)lstQualityFiles).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstQualityFiles).set_FormattingEnabled(true);
			((Control)lstQualityFiles).set_Location(new Point(9, 88));
			((Control)lstQualityFiles).set_Name("lstQualityFiles");
			((Control)lstQualityFiles).set_Size(new Size(102, 121));
			((Control)lstQualityFiles).set_TabIndex(18);
			((Control)cmdAccept).set_Location(new Point(147, 90));
			((Control)cmdAccept).set_Name("cmdAccept");
			((Control)cmdAccept).set_Size(new Size(61, 29));
			((Control)cmdAccept).set_TabIndex(20);
			((Control)cmdAccept).set_Text("Accept");
			((ButtonBase)cmdAccept).set_UseVisualStyleBackColor(true);
			((Control)cmdAccept).add_Click((EventHandler)Accept_Click);
			((Control)cmdCancel).set_Location(new Point(147, 134));
			((Control)cmdCancel).set_Name("cmdCancel");
			((Control)cmdCancel).set_Size(new Size(61, 29));
			((Control)cmdCancel).set_TabIndex(21);
			((Control)cmdCancel).set_Text("Cancel");
			((ButtonBase)cmdCancel).set_UseVisualStyleBackColor(true);
			((Control)cmdCancel).add_Click((EventHandler)cmdCancel_Click);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(228, 224));
			((Form)this).set_ControlBox(false);
			((Control)this).get_Controls().Add((Control)(object)cmdCancel);
			((Control)this).get_Controls().Add((Control)(object)cmdAccept);
			((Control)this).get_Controls().Add((Control)(object)label22);
			((Control)this).get_Controls().Add((Control)(object)lstQualityFiles);
			((Control)this).get_Controls().Add((Control)(object)label3);
			((Control)this).get_Controls().Add((Control)(object)label2);
			((Control)this).get_Controls().Add((Control)(object)label1);
			((Control)this).get_Controls().Add((Control)(object)txtFileName);
			((Form)this).set_FormBorderStyle((FormBorderStyle)3);
			((Control)this).set_Name("SetCompileFileNames");
			((Control)this).set_Text("Set File Name for Compilation file");
			((Form)this).add_Load((EventHandler)SetCompileFileNames_Load);
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
