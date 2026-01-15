using System;
using System.ComponentModel;
using System.Drawing;
using System.IO;
using System.Windows.Forms;

namespace Occult.Asteroid_Observations
{
	public class Unistellar_CopyObservations : Form
	{
		private static string Unistellar_Directory;

		private static DirectoryInfo Di;

		private IContainer components;

		private TextBox txtFileContent;

		private TextBox txtCurrentfile;

		private Label label1;

		private Button cmdMarkAsProcessed;

		private Button cmdExit;

		public Unistellar_CopyObservations(string Ev_directory)
		{
			InitializeComponent();
			Unistellar_Directory = Ev_directory;
			ListFileContent();
		}

		private void cmdExit_Click(object sender, EventArgs e)
		{
			CloseForm();
		}

		internal void CloseForm()
		{
			((Form)this).Close();
			((Component)this).Dispose();
		}

		private void ListFileContent()
		{
			//IL_003d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0144: Unknown result type (might be due to invalid IL or missing references)
			Di = new DirectoryInfo(Unistellar_Directory);
			((Control)txtFileContent).set_Text("");
			Application.DoEvents();
			if (!Directory.Exists(Unistellar_Directory))
			{
				MessageBox.Show("Before you can run this process, you must first 'sort' the Unistellar observations", "Observations not sorted", (MessageBoxButtons)0, (MessageBoxIcon)48);
				CloseForm();
				return;
			}
			string[] files = Directory.GetFiles(Unistellar_Directory, "*.xml");
			foreach (string text in files)
			{
				if (!text.Contains("done.xml"))
				{
					((Control)txtCurrentfile).set_Text(text);
					using (StreamReader streamReader = new StreamReader(text))
					{
						((Control)txtFileContent).set_Text(streamReader.ReadToEnd());
					}
					((Control)txtFileContent).Focus();
					Application.DoEvents();
					((TextBoxBase)txtFileContent).set_SelectionStart(((Control)txtFileContent).get_Text().IndexOf(Tags.TagStart[18], 20));
					int startIndex = ((Control)txtFileContent).get_Text().IndexOf(Tags.TagEnd[18]);
					startIndex = ((Control)txtFileContent).get_Text().IndexOf(">", startIndex) + 1;
					((TextBoxBase)txtFileContent).set_SelectionLength(startIndex - ((TextBoxBase)txtFileContent).get_SelectionStart());
					return;
				}
			}
			MessageBox.Show("All files have been processed\r\n\r\nThe form will now close.", "All files processed", (MessageBoxButtons)0, (MessageBoxIcon)64);
			CloseForm();
		}

		private void cmdMarkAsProcessed_Click(object sender, EventArgs e)
		{
			string text = ((Control)txtCurrentfile).get_Text();
			string destFileName = text.Replace(".xml", "_done.xml");
			File.Move(text, destFileName);
			ListFileContent();
		}

		private void Unistellar_CopyObservations_Resize(object sender, EventArgs e)
		{
			if (((Control)this).get_Width() < 400)
			{
				((Control)this).set_Width(400);
			}
			if (((Control)this).get_Height() < 400)
			{
				((Control)this).set_Height(400);
			}
			((Control)txtFileContent).set_Width(((Control)this).get_Width() - 40);
			((Control)txtFileContent).set_Height(((Control)this).get_Height() - 145);
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
			txtFileContent = new TextBox();
			txtCurrentfile = new TextBox();
			label1 = new Label();
			cmdMarkAsProcessed = new Button();
			cmdExit = new Button();
			((Control)this).SuspendLayout();
			((Control)txtFileContent).set_Font(new Font("Consolas", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtFileContent).set_Location(new Point(12, 45));
			((TextBoxBase)txtFileContent).set_Multiline(true);
			((Control)txtFileContent).set_Name("txtFileContent");
			txtFileContent.set_ScrollBars((ScrollBars)3);
			((Control)txtFileContent).set_Size(new Size(693, 631));
			((Control)txtFileContent).set_TabIndex(0);
			((TextBoxBase)txtFileContent).set_WordWrap(false);
			((Control)txtCurrentfile).set_Location(new Point(101, 9));
			((Control)txtCurrentfile).set_Name("txtCurrentfile");
			((TextBoxBase)txtCurrentfile).set_ReadOnly(true);
			((Control)txtCurrentfile).set_Size(new Size(601, 20));
			((Control)txtCurrentfile).set_TabIndex(1);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label1).set_Location(new Point(21, 12));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(80, 15));
			((Control)label1).set_TabIndex(2);
			((Control)label1).set_Text("Displayed file");
			((Control)cmdMarkAsProcessed).set_Anchor((AnchorStyles)2);
			((Control)cmdMarkAsProcessed).set_Location(new Point(193, 686));
			((Control)cmdMarkAsProcessed).set_Name("cmdMarkAsProcessed");
			((Control)cmdMarkAsProcessed).set_Size(new Size(140, 43));
			((Control)cmdMarkAsProcessed).set_TabIndex(3);
			((Control)cmdMarkAsProcessed).set_Text("Mark file as Processed\r\nand display next file");
			((ButtonBase)cmdMarkAsProcessed).set_UseVisualStyleBackColor(true);
			((Control)cmdMarkAsProcessed).add_Click((EventHandler)cmdMarkAsProcessed_Click);
			((Control)cmdExit).set_Anchor((AnchorStyles)2);
			((Control)cmdExit).set_Location(new Point(425, 686));
			((Control)cmdExit).set_Name("cmdExit");
			((Control)cmdExit).set_Size(new Size(98, 43));
			((Control)cmdExit).set_TabIndex(4);
			((Control)cmdExit).set_Text("Exit");
			((ButtonBase)cmdExit).set_UseVisualStyleBackColor(true);
			((Control)cmdExit).add_Click((EventHandler)cmdExit_Click);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(717, 737));
			((Control)this).get_Controls().Add((Control)(object)cmdExit);
			((Control)this).get_Controls().Add((Control)(object)cmdMarkAsProcessed);
			((Control)this).get_Controls().Add((Control)(object)label1);
			((Control)this).get_Controls().Add((Control)(object)txtCurrentfile);
			((Control)this).get_Controls().Add((Control)(object)txtFileContent);
			((Control)this).set_Name("eVscope_CopyObservations");
			((Control)this).set_Text("eVscope_CopyObservations");
			((Control)this).add_Resize((EventHandler)Unistellar_CopyObservations_Resize);
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
