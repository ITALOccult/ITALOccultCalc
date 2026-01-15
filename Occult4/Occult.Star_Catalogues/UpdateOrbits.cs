using System;
using System.Collections;
using System.IO;
using System.Text;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult.Star_Catalogues
{
	internal class UpdateOrbits
	{
		internal static void UpdateXZOrbits()
		{
			//IL_0034: Unknown result type (might be due to invalid IL or missing references)
			//IL_00ea: Unknown result type (might be due to invalid IL or missing references)
			//IL_00f0: Invalid comparison between Unknown and I4
			//IL_00fd: Unknown result type (might be due to invalid IL or missing references)
			//IL_07b5: Unknown result type (might be due to invalid IL or missing references)
			bool flag = false;
			TimeSpan zero = TimeSpan.Zero;
			int num = 0;
			ArrayList arrayList = new ArrayList();
			if (File.Exists(Utilities.AppPath + "\\Resource Files\\XZDoubles.dat.bup"))
			{
				MessageBox.Show("To protect against loss of data, the current version of XZDoubles.dat\r\nwill be renamed to XZDoubles.dat.bup. \r\n\r\nYou need to MANUALLY DELETE the existing\r\nversion of XZDoubles.dat.bup before this routine will run. \r\nThis will allow you to make a back-up copy of the earlier version of XZdoubles.dat", "Delete a file");
				return;
			}
			bool flag2 = true;
			if (File.Exists(Utilities.AppPath + "\\DownLoaded Files\\SixthOrbitCatalogue.txt"))
			{
				flag = true;
				DateTime lastWriteTime = File.GetLastWriteTime(Utilities.AppPath + "\\DownLoaded Files\\SixthOrbitCatalogue.txt");
				num = (DateTime.Now - lastWriteTime).Days;
				if (num < 100)
				{
					flag2 = false;
				}
			}
			if (flag2 && !http.DownloadHTTP(Settings.Default.Orb6DownloadServer, Settings.Default.Orb6DownloadFile, Utilities.AppPath + "\\DownLoaded Files\\SixthOrbitCatalogue.txt", unzip: false, gunzip: false, ShowMessages: false))
			{
				if (!flag)
				{
					MessageBox.Show("Download of the 6th catalogue of Orbits of Visual Binary Stars failed,\r\nand you do not have an old copy of the file. ", "Failed download");
					return;
				}
				if ((int)MessageBox.Show("Download of the 6th catalogue of Orbits of Visual Binary Stars failed.\r\n\r\nYou have a copy of that catalogue that is " + num + " days old.\r\n\r\nDo you want to proceed using that version?", "Failed download", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 7)
				{
					return;
				}
			}
			using (StreamWriter streamWriter = new StreamWriter(Utilities.AppPath + "\\DownLoaded Files\\SixthOrbitEclipticSubset.txt"))
			{
				using StreamReader streamReader = new StreamReader(Utilities.AppPath + "\\DownLoaded Files\\SixthOrbitCatalogue.txt");
				do
				{
					string text = streamReader.ReadLine();
					if (text.Substring(0, 6) == "000000")
					{
						continue;
					}
					if (!double.TryParse(text.Substring(19, 2), out var result))
					{
						result = 0.0;
					}
					if (!double.TryParse(text.Substring(21, 2) + "." + text.Substring(23, 1), out var result2))
					{
						result2 = 0.0;
					}
					double a = 15.0 * (result + result2 / 60.0) / (180.0 / Math.PI);
					if (!double.TryParse(text.Substring(25, 2), out var result3))
					{
						result3 = 80.0;
					}
					if (!double.TryParse(text.Substring(27, 2), out var result4))
					{
						result4 = 0.0;
					}
					double num2 = (result3 + result4 / 60.0) / (180.0 / Math.PI);
					if (text.Substring(24, 1) == "-")
					{
						num2 = 0.0 - num2;
					}
					if (!(Math.Abs((0.0 - Math.Cos(num2)) * Math.Sin(a) * Utilities.sinEcliptic2000[Utilities.EclipticID] + Math.Sin(num2) * Utilities.cosEcliptic2000[Utilities.EclipticID]) < 0.121869))
					{
						continue;
					}
					StringBuilder stringBuilder = new StringBuilder();
					stringBuilder.Append(text.Substring(19, 11));
					int result5;
					if (text.Substring(30, 3) == "SAO")
					{
						stringBuilder.Append("S" + text.Substring(34, 6).Trim().PadLeft(6) + " ".PadRight(6));
					}
					else if (text.Substring(30, 2) == "GC")
					{
						stringBuilder.Append("GC" + text.Substring(33, 5).Trim().PadLeft(5) + " ".PadRight(6));
					}
					else if (int.TryParse(text.Substring(37, 1), out result5))
					{
						stringBuilder.Append(text.Substring(30, 8).Replace(" ", "").PadRight(7) + " ".PadRight(6));
					}
					else
					{
						string text2 = text.Substring(30, 7);
						if ((text2.Substring(1, 1) == " ") & (text2.Substring(3, 1) == " "))
						{
							text2 = text2.Substring(0, 1) + "   " + text2.Substring(4);
						}
						stringBuilder.Append(text2 + " " + text.Substring(37, 6).Trim().PadRight(5));
					}
					stringBuilder.Append(" | ");
					double result6;
					if (text.Substring(92, 1) == "y")
					{
						stringBuilder.Append(" " + text.Substring(81, 10));
					}
					else if (text.Substring(92, 1) == "c")
					{
						if (!double.TryParse(text.Substring(81, 10), out result6))
						{
							result6 = 0.0;
						}
						stringBuilder.AppendFormat("{0,11:F5}", result6 * 100.0);
					}
					else if (text.Substring(92, 1) == "d")
					{
						if (!double.TryParse(text.Substring(81, 10), out result6))
						{
							result6 = 0.0;
						}
						stringBuilder.AppendFormat("{0,11:F5}", result6 / 365.25);
					}
					else
					{
						stringBuilder.Append(" ".PadRight(11));
					}
					if (text.Substring(114, 1) == "a")
					{
						stringBuilder.Append(text.Substring(105, 9) + " ");
					}
					else
					{
						if (!double.TryParse(text.Substring(105, 9), out result6))
						{
							result6 = 0.0;
						}
						stringBuilder.AppendFormat("{0,10:F6}", result6 / 1000.0);
					}
					stringBuilder.Append(text.Substring(124, 9));
					stringBuilder.Append(text.Substring(142, 9));
					if (text.Substring(174, 1) == "y")
					{
						stringBuilder.Append(text.Substring(162, 11));
					}
					else
					{
						if (!double.TryParse(text.Substring(162, 12), out result6))
						{
							result6 = 0.0;
						}
						stringBuilder.AppendFormat("{0,11:F5}", 1900.0 + (result6 - 15020.0) / 365.25);
					}
					stringBuilder.Append(text.Substring(186, 8));
					stringBuilder.Append(text.Substring(204, 9));
					streamWriter.WriteLine(stringBuilder.ToString());
				}
				while (!streamReader.EndOfStream);
			}
			arrayList.Clear();
			using (StreamReader streamReader2 = new StreamReader(Utilities.AppPath + "\\Resource Files\\XZDoubles.dat"))
			{
				do
				{
					arrayList.Add(streamReader2.ReadLine());
				}
				while (!streamReader2.EndOfStream);
			}
			int i = 0;
			int num3 = 0;
			using (StreamWriter streamWriter2 = new StreamWriter(Utilities.AppPath + "\\DownLoaded Files\\SixthOrbit_notMatched.txt"))
			{
				using StreamReader streamReader3 = new StreamReader(Utilities.AppPath + "\\DownLoaded Files\\SixthOrbitEclipticSubset.txt");
				do
				{
					string text = streamReader3.ReadLine();
					string text3 = text.Substring(0, 24);
					num3 = int.Parse(text3.Substring(0, 5));
					i -= 10;
					if (i < 0)
					{
						i = 0;
					}
					for (; i < arrayList.Count; i++)
					{
						string text4 = arrayList[i]!.ToString();
						string text5 = text4.Substring(21, 24);
						if (int.Parse(text5.Substring(0, 5)) > num3)
						{
							streamWriter2.WriteLine(text);
							break;
						}
						if (text5 == text3)
						{
							arrayList[i] = text4.Substring(0, 131) + text.Substring(27);
							break;
						}
					}
				}
				while (!streamReader3.EndOfStream);
			}
			if (File.Exists(Utilities.AppPath + "\\Resource Files\\XZDoubles.dat.bup"))
			{
				MessageBox.Show("You have not deleted the file 'XZDoubles.dat.bup' from \r\nthe Resource Files directory. You should keepa copy of this file \r\nin case of update error. The current XZDoubles.dat file will be\r\nrenamed to XZDoubles.dat.bup", "Haven't deleted backup");
				return;
			}
			File.Copy(Utilities.AppPath + "\\Resource Files\\XZDoubles.dat", Utilities.AppPath + "\\Resource Files\\XZDoubles.dat.bup");
			using StreamWriter streamWriter3 = new StreamWriter(Utilities.AppPath + "\\Resource Files\\XZDoubles.dat");
			for (int j = 0; j < arrayList.Count; j++)
			{
				streamWriter3.WriteLine(arrayList[j]!.ToString());
			}
		}
	}
}
